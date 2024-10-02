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

import Relude

import Cardano.Address (NetworkTag)
import Cardano.Address.Style.Shelley (shelleyMainnet, shelleyTestnet)
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Ledger.Address qualified as Ledger
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Mnemonic (MkMnemonicError)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Except (except, withExceptT)
import Data.List.NonEmpty qualified as NE
import Data.Tagged (Tagged, untag)
import NoThunks.Class.Extended (NoThunks)
import Ouroboros.Network.Magic (NetworkMagic, unNetworkMagic)
import Path (Abs, File, Path)
import Yare.Address.Derivation
  ( AddressWithKey (..)
  , externalPaymentAdressesKeys
  )
import Yare.Address.Derivation qualified as Derivation
import Yare.Chain.Types (LedgerAddress)
import Yare.Mnemonic (mnemonicFromFile)

newtype Addresses = Addresses {externalAddresses ∷ NonEmpty AddressWithKey}
  deriving stock (Generic)
  deriving anyclass (NoThunks)

deriveFromMnemonic
  ∷ MonadIO m
  ⇒ NetworkMagic
  → Tagged "mnemonic" (Path Abs File)
  → m (Either Error Addresses)
deriveFromMnemonic networkMagic mnemonicFile = runExceptT do
  nt ← except $ networkMagicToAddressesTag networkMagic
  mnemonic ←
    mnemonicFromFile (untag mnemonicFile)
      & ExceptT
      & withExceptT MnemonicError
  externalLedgerAddrs ∷ NonEmpty AddressWithKey ← do
    keys ←
      externalPaymentAdressesKeys nt mnemonic
        & except
        & withExceptT DerivationError
    -- At least 2 addresses are needed:
    -- 1 for change, fees and collateral and 1 for the ref script.
    NE.nonEmpty (take 20 keys)
      & maybe (throwError NoAddressesDerived) pure
  pure Addresses {externalAddresses = force externalLedgerAddrs}

isOwnAddress ∷ Addresses → LedgerAddress → Bool
isOwnAddress Addresses {externalAddresses} address =
  address `elem` fmap ledgerAddress externalAddresses

useForChange ∷ Addresses → (Addresses, AddressWithKey)
useForChange a@Addresses {externalAddresses} =
  -- While the function type makes it possible to modify the addresses state,
  -- we don't do it in the current implementation always using the same and the
  -- only external address for change in order to KISS.
  (a, NE.head externalAddresses)

useForFees ∷ Addresses → (Addresses, AddressWithKey)
useForFees a@Addresses {externalAddresses} =
  -- While the function type makes it possible to modify the addresses state,
  -- we don't do it in the current implementation always using the same and the
  -- only external address for fees in order to KISS.
  (a, NE.head externalAddresses)

useForCollateral ∷ Addresses → (Addresses, AddressWithKey)
useForCollateral a@Addresses {externalAddresses} =
  -- While the function type makes it possible to modify the addresses state,
  -- we don't do it in the current implementation always using the same and the
  -- only external address for collateral in order to KISS.
  (a, NE.last externalAddresses)

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
  deriving stock (Eq, Show)
