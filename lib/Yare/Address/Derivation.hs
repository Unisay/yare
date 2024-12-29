module Yare.Address.Derivation
  ( externalPaymentAdressesKeys
  , internalPaymentAdressesKeys
  , AddressWithKey (..)
  , toLedgerAddress
  , toPaymentCredential
  , Error (..)
  ) where

import Yare.Prelude hiding (show)

import Cardano.Address (Address, NetworkTag (..), unAddress)
import Cardano.Address.Derivation
  ( Depth (AccountK, PaymentK, RootK)
  , DerivationType (Hardened, Soft)
  , HardDerivation
    ( AddressIndexDerivationType
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    )
  , Index
  , XPrv
  , nextIndex
  , toXPub
  )
import Cardano.Address.Style.Shelley qualified as CAddr
import Cardano.Crypto.Wallet qualified as Crypto
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Api (StandardCrypto)
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Credential (PaymentCredential)
import Cardano.Mnemonic (Mnemonic, SomeMnemonic (..))
import Codec.Serialise.Class.Orphans ()
import Data.Traversable (for)
import Fmt (Buildable (..))
import Fmt.Orphans ()
import NoThunks.Class.Extended (NoThunks)
import Text.Show (show)
import Yare.Chain.Types (LedgerAddress)

data AddressWithKey = MkAddressWithKey
  { ledgerAddress ∷ !LedgerAddress
  , paymentKey ∷ !Crypto.XPrv
  }
  deriving stock (Generic)
  deriving anyclass (NoThunks, NFData)

instance Buildable AddressWithKey where
  build MkAddressWithKey {ledgerAddress} =
    build ledgerAddress
      <> "\n"
      <> build (toPaymentCredential ledgerAddress)

externalPaymentAdressesKeys
  ∷ NetworkTag
  → Mnemonic 24
  → Either Error [AddressWithKey]
externalPaymentAdressesKeys = paymentAddressesKeys CAddr.UTxOExternal

internalPaymentAdressesKeys
  ∷ NetworkTag
  → Mnemonic 24
  → Either Error [AddressWithKey]
internalPaymentAdressesKeys = paymentAddressesKeys CAddr.UTxOInternal

paymentAddressesKeys
  ∷ CAddr.Role
  → NetworkTag
  → Mnemonic 24
  → Either Error [AddressWithKey]
paymentAddressesKeys role networkTag mnemonic =
  for (take 20 paymentKeyIxs) makePaymentAddressWithKey
 where
  paymentKeyIxs ∷ [Index (AddressIndexDerivationType CAddr.Shelley) PaymentK] =
    let firstIx ∷ Index Soft PaymentK = minBound
     in firstIx : unfoldr (fmap (\a → (a, a)) . nextIndex) firstIx

  makePaymentAddressWithKey
    ∷ HasCallStack
    ⇒ Index (AddressIndexDerivationType CAddr.Shelley) PaymentK
    → Either Error AddressWithKey
  makePaymentAddressWithKey paymentAddrIx = do
    ledgerAddress ← toLedgerAddress cardanoAddress
    pure MkAddressWithKey {ledgerAddress, paymentKey = CAddr.getKey paymentKey}
   where
    cardanoAddress ∷ Address =
      CAddr.paymentAddress networkTag $
        CAddr.PaymentFromExtendedKey (toXPub <$> paymentKey)

    paymentKey ∷ CAddr.Shelley PaymentK XPrv =
      deriveAddressPrivateKey deriveShelleyAccountKey role paymentAddrIx

  masterKey ∷ CAddr.Shelley RootK XPrv =
    CAddr.genMasterKeyFromMnemonic (SomeMnemonic mnemonic) mempty

  deriveShelleyAccountKey ∷ CAddr.Shelley AccountK XPrv =
    deriveAccountPrivateKey masterKey accountIx

  accountIx ∷ Index 'Hardened 'AccountK =
    minBound

toLedgerAddress ∷ Address → Either Error LedgerAddress
toLedgerAddress address =
  maybeToRight (ToLedgerAddrConversionError address) $
    Ledger.decodeAddr (unAddress address)

toPaymentCredential ∷ LedgerAddress → Maybe (PaymentCredential StandardCrypto)
toPaymentCredential = \case
  Addr _ paymentCredential _ → Just paymentCredential
  AddrBootstrap {} → Nothing

--------------------------------------------------------------------------------
-- Errors ----------------------------------------------------------------------

data Error where
  XPrvConversionError ∷ Error
  ToLedgerAddrConversionError ∷ Address → Error
  deriving stock (Eq)

instance Show Error where
  show = \case
    XPrvConversionError →
      "Failed to convert `Cardano.Address.XPrv` to `Ledger.SignKeyDSIGN`"
    ToLedgerAddrConversionError addr →
      "Failed to convert `Cardano.Address.Address` to `Ledger.Addr`: "
        <> show addr
