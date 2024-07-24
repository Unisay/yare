module Yare.Addresses
  ( Addresses
  , deriveFromMnemonic
  , isOwnAddress
  , useChangeAddress
  , Error (..)
  , networkMagicToLedgerNetwork
  , networkMagicToAddressesTag
  ) where

import Relude

import Cardano.Address (NetworkTag)
import Cardano.Address.Style.Shelley (shelleyMainnet, shelleyTestnet)
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Api (StandardCrypto)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Credential (PaymentCredential)
import Cardano.Mnemonic (MkMnemonicError)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Oops (CouldBe, CouldBeAnyOf)
import Control.Monad.Oops qualified as Oops
import Data.Set (member)
import Data.Set qualified as Set
import Data.Tagged (Tagged, untag)
import Ouroboros.Network.Magic (NetworkMagic, unNetworkMagic)
import Path (Abs, File, Path)
import Relude.Unsafe qualified as Unsafe
import Yare.Address
  ( AddressWithKey (address)
  , externalPaymentAdressesKeys
  , internalPaymentAdressesKeys
  , toLedgerAddress
  )
import Yare.Chain.Types (LedgerAddress)
import Yare.Mnemonic (mnemonicFromFile)

type Addresses ∷ Type
data Addresses = Addresses
  { internalAddress ∷ LedgerAddress
  , externalAddress ∷ LedgerAddress
  , paymentCredentials ∷ Set (PaymentCredential StandardCrypto)
  }

deriveFromMnemonic
  ∷ ( MonadError (Oops.Variant e) m
    , e `CouldBeAnyOf` [Error, MkMnemonicError 8]
    , MonadIO m
    )
  ⇒ NetworkMagic
  → Tagged "mnemonic" (Path Abs File)
  → m Addresses
deriveFromMnemonic networkMagic mnemonicFile = do
  nt ← networkMagicToAddressesTag networkMagic
  mnemonic ← mnemonicFromFile (untag mnemonicFile)
  let oneAddress pool =
        fromMaybe (error "Failed to parse address")
          . toLedgerAddress
          . address
          . Unsafe.head
          $ pool nt mnemonic
      internalAddress = oneAddress internalPaymentAdressesKeys
      externalAddress = oneAddress externalPaymentAdressesKeys
  pure
    Addresses
      { internalAddress
      , externalAddress
      , paymentCredentials =
          Set.fromList
            [ Unsafe.fromJust $ shelleyPaymentCred internalAddress
            , Unsafe.fromJust $ shelleyPaymentCred externalAddress
            ]
      }

isOwnAddress ∷ Addresses → LedgerAddress → Bool
isOwnAddress Addresses {paymentCredentials} address =
  case shelleyPaymentCred address of
    Nothing → False
    Just paymentCredential → paymentCredential `member` paymentCredentials

useChangeAddress ∷ Addresses → (Addresses, LedgerAddress)
useChangeAddress a@Addresses {internalAddress} =
  -- Not actually modifying the addresses state
  (a, internalAddress)

shelleyPaymentCred ∷ LedgerAddress → Maybe (PaymentCredential StandardCrypto)
shelleyPaymentCred = \case
  AddrBootstrap _bootstrapAddress → Nothing
  Addr _net paymentCred _stakeRef → Just paymentCred

networkMagicToAddressesTag
  ∷ (MonadError (Oops.Variant e) m, e `CouldBe` Error)
  ⇒ NetworkMagic
  → m NetworkTag
networkMagicToAddressesTag =
  unNetworkMagic >>> \case
    764824073 → pure shelleyMainnet -- https://bityl.co/LitF
    1 → pure shelleyTestnet
    2 → pure shelleyTestnet
    n → Oops.throw (NetworkMagicNoTag n)

networkMagicToLedgerNetwork
  ∷ (MonadError (Oops.Variant e) m, e `CouldBe` Error)
  ⇒ NetworkMagic
  → m Ledger.Network
networkMagicToLedgerNetwork =
  unNetworkMagic >>> \case
    764824073 → pure Ledger.Mainnet
    1 → pure Ledger.Testnet
    2 → pure Ledger.Testnet
    n → Oops.throw (NetworkMagicNoTag n)

--------------------------------------------------------------------------------
-- Errors ----------------------------------------------------------------------

type Error ∷ Type
newtype Error = NetworkMagicNoTag Word32
  deriving stock (Eq, Show)
