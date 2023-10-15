module Yare.Addresses
  ( Addresses
  , deriveFromMnemonic
  , isOwnAddress
  , Error (..)
  ) where

import Relude

import Cardano.Address (NetworkTag)
import Cardano.Address.Style.Shelley (shelleyMainnet, shelleyTestnet)
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Api (StandardCrypto)
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
import Yare.Address
  ( AddressWithKey (AddressWithKey, address)
  , externalPaymentAdressesKeys
  , internalPaymentAdressesKeys
  , toLedgerAddress
  )
import Yare.Chain.Types (LedgerAddress)
import Yare.Mnemonic (mnemonicFromFile)

data Addresses = Addresses
  { paymentAddresses ∷ Set LedgerAddress
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
  nt ← networkMagicToTag networkMagic
  mnemonic ← mnemonicFromFile (untag mnemonicFile)
  let unsafeToLedgerAddr =
        toLedgerAddress >>> fromMaybe (error "Failed to parse address")
      externalAddresses ∷ [LedgerAddress] = take 20 do
        AddressWithKey {address} ← externalPaymentAdressesKeys nt mnemonic
        [unsafeToLedgerAddr address]
      internalAddresses ∷ [LedgerAddress] = take 20 do
        AddressWithKey {address} ← internalPaymentAdressesKeys nt mnemonic
        [unsafeToLedgerAddr address]
      paymentAddresses ∷ [LedgerAddress] =
        externalAddresses ++ internalAddresses
  pure
    Addresses
      { paymentAddresses =
          Set.fromList paymentAddresses
      , paymentCredentials =
          Set.fromList (mapMaybe shelleyPaymentCred paymentAddresses)
      }

isOwnAddress ∷ Addresses → LedgerAddress → Bool
isOwnAddress Addresses {paymentCredentials} address =
  case shelleyPaymentCred address of
    Nothing → False
    Just paymentCredential → paymentCredential `member` paymentCredentials

shelleyPaymentCred ∷ LedgerAddress → Maybe (PaymentCredential StandardCrypto)
shelleyPaymentCred = \case
  AddrBootstrap _bootstrapAddress → Nothing
  Addr _net paymentCred _stakeRef → Just paymentCred

networkMagicToTag
  ∷ (MonadError (Oops.Variant e) m, e `CouldBe` Error)
  ⇒ NetworkMagic
  → m NetworkTag
networkMagicToTag =
  unNetworkMagic >>> \case
    764824073 → pure shelleyMainnet -- https://bityl.co/LitF
    1 → pure shelleyTestnet
    2 → pure shelleyTestnet
    n → Oops.throw (NetworkMagicNoTag n)

--------------------------------------------------------------------------------
-- Errors ----------------------------------------------------------------------

newtype Error = NetworkMagicNoTag Word32
  deriving stock (Eq, Show)
