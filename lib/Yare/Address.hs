module Yare.Address
  ( module Derivation
  , Addresses
  , externalAddresses
  , deriveFromMnemonic
  , isOwnAddress
  , useForChange
  , useForFees
  , useForCollateral
  , forScript
  , networkMagicToLedgerNetwork
  , networkMagicToAddressesTag
  , Error (..)
  ) where

import Yare.Prelude

import Cardano.Address (NetworkTag)
import Cardano.Address.Style.Shelley (shelleyMainnet, shelleyTestnet)
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Ledger.Api (Addr (..), StandardCrypto)
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Credential (PaymentCredential)
import Cardano.Mnemonic (MkMnemonicError)
import Codec.Serialise.Class.Orphans ()
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Except (except, withExceptT)
import Data.List.NonEmpty qualified as NE
import Fmt (Buildable (build), blockListF, nameF)
import NoThunks.Class.Extended (NoThunks)
import Ouroboros.Network.Magic (NetworkMagic (..), unNetworkMagic)
import Path (Abs, File, Path)
import Yare.Address.Derivation
  ( AddressWithKey (..)
  , externalPaymentAdressesKeys
  , internalPaymentAdressesKeys
  )
import Yare.Address.Derivation qualified as Derivation
import Yare.Chain.Types (LedgerAddress)
import Yare.Mnemonic (mnemonicFromFile)

data Addresses = Addresses
  { network ∷ !Ledger.Network
  , externalAddresses ∷ !(NonEmpty AddressWithKey)
  , internalAddresses ∷ !(NonEmpty AddressWithKey)
  , paymentCredentials ∷ ![PaymentCredential StandardCrypto]
  -- ^ Payment credentials of the external addresses cached for faster lookups.
  }
  deriving stock (Generic)
  deriving anyclass (NoThunks)

instance Buildable Addresses where
  build Addresses {network, externalAddresses, internalAddresses} =
    nameF "Addresses" $
      blockListF
        [ nameF "Network" (show network)
        , nameF "Internal addresses" (blockListF internalAddresses)
        , nameF "External addresses" (blockListF externalAddresses)
        ]

deriveFromMnemonic
  ∷ MonadIO m
  ⇒ NetworkMagic
  → Tagged "mnemonic" (Path Abs File)
  → m (Either Error Addresses)
deriveFromMnemonic networkMagic mnemonicFile = runExceptT do
  networkTag ← except $ networkMagicToAddressesTag networkMagic
  ledgerNetwork ← except $ networkMagicToLedgerNetwork networkMagic
  mnemonic ←
    mnemonicFromFile (untag mnemonicFile)
      & ExceptT
      & withExceptT MnemonicError
  let deriveAddresses paymentAddrKeys = do
        keys ←
          paymentAddrKeys networkTag mnemonic
            & except
            & withExceptT DerivationError
        -- At least 2 addresses are needed:
        -- 1 for change, fees and collateral and 1 for the ref script.
        NE.nonEmpty (take 20 keys)
          & maybe (throwError NoAddressesDerived) pure

  externalLedgerAddrs ∷ NonEmpty AddressWithKey ←
    deriveAddresses externalPaymentAdressesKeys

  internalLedgerAddrs ∷ NonEmpty AddressWithKey ←
    deriveAddresses internalPaymentAdressesKeys

  pure
    Addresses
      { network = ledgerNetwork
      , externalAddresses = force externalLedgerAddrs
      , internalAddresses = force internalLedgerAddrs
      , paymentCredentials =
          force
            [ cred
            | AddressWithKey {ledgerAddress} ←
                toList externalLedgerAddrs <> toList internalLedgerAddrs
            , cred ← case ledgerAddress of
                Addr _ paymentCredential _ → [paymentCredential]
                AddrBootstrap {} → []
            ]
      }

{- | Checks if the given address is one of the own addresses.
The address is considered own if its payment credential matches one of the
payment credentials of our external addresses. This way staking credentials
are not considered.
-}
isOwnAddress ∷ Addresses → LedgerAddress → Bool
isOwnAddress Addresses {network, paymentCredentials} = \case
  AddrBootstrap {} → False
  Addr addrNnetwork paymentCred _stakeCred →
    network == addrNnetwork && paymentCred `elem` paymentCredentials

useForChange ∷ Addresses → AddressWithKey
useForChange Addresses {externalAddresses} = NE.head externalAddresses

useForFees ∷ Addresses → AddressWithKey
useForFees Addresses {externalAddresses} = NE.head externalAddresses

useForCollateral ∷ Addresses → AddressWithKey
useForCollateral Addresses {externalAddresses} = NE.last externalAddresses

forScript
  ∷ Ledger.Network
  → Ledger.ScriptHash Ledger.StandardCrypto
  → LedgerAddress
forScript network scriptHash =
  Ledger.Addr network (Ledger.ScriptHashObj scriptHash) Ledger.StakeRefNull

networkMagicToAddressesTag ∷ NetworkMagic → Either Error NetworkTag
networkMagicToAddressesTag =
  unNetworkMagic >>> \case
    764824073 → Right shelleyMainnet -- https://bityl.co/LitF
    1 → Right shelleyTestnet
    2 → Right shelleyTestnet
    n → Left (NetworkMagicNoTag n)

networkMagicToLedgerNetwork ∷ NetworkMagic → Either Error Ledger.Network
networkMagicToLedgerNetwork =
  unNetworkMagic >>> \case
    764824073 → Right Ledger.Mainnet
    1 → Right Ledger.Testnet
    2 → Right Ledger.Testnet
    n → Left (NetworkMagicNoTag n)

--------------------------------------------------------------------------------
-- Errors ----------------------------------------------------------------------

type Error ∷ Type
data Error
  = NetworkMagicNoTag Word32
  | MnemonicError (MkMnemonicError 8)
  | DerivationError Derivation.Error
  | NoAddressesDerived
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Exception)
