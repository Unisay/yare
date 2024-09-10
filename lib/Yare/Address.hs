module Yare.Address
  ( externalPaymentAdressesKeys
  , internalPaymentAdressesKeys
  , AddressWithKey (..)
  , toLedgerAddress
  ) where

import Relude

import Cardano.Address (Address, NetworkTag (..), unAddress)
import Cardano.Address.Derivation
  ( Depth (AccountK, PaymentK)
  , DerivationType (Hardened, Soft)
  , GenMasterKey (genMasterKeyFromMnemonic)
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
import Cardano.Address.Style.Shelley
  ( Credential (..)
  , Role (..)
  , Shelley
  , paymentAddress
  )
import Cardano.Ledger.Api.Tx.Address qualified as Ledger
import Cardano.Mnemonic (Mnemonic, SomeMnemonic (..))
import NoThunks.Class.Extended (NoThunks)
import Yare.Chain.Types (LedgerAddress)

type AddressWithKey ∷ Type
data AddressWithKey = AddressWithKey
  { cardanoAddress ∷ Address
  , ledgerAddress ∷ LedgerAddress
  , key ∷ Shelley PaymentK XPrv
  }
  deriving stock (Generic)
  deriving anyclass (NFData, NoThunks)

externalPaymentAdressesKeys ∷ NetworkTag → Mnemonic 24 → [AddressWithKey]
externalPaymentAdressesKeys = paymentAddressesKeys UTxOExternal

internalPaymentAdressesKeys ∷ NetworkTag → Mnemonic 24 → [AddressWithKey]
internalPaymentAdressesKeys = paymentAddressesKeys UTxOInternal

paymentAddressesKeys ∷ Role → NetworkTag → Mnemonic 24 → [AddressWithKey]
paymentAddressesKeys role networkTag mnemonic =
  paymentKeyIxs <&> makePaymentAddressWithKey
 where
  paymentKeyIxs ∷ [Index (AddressIndexDerivationType Shelley) PaymentK] =
    let firstIx ∷ Index Soft PaymentK = minBound
     in firstIx : unfoldr (fmap (\a → (a, a)) . nextIndex) firstIx

  makePaymentAddressWithKey
    ∷ HasCallStack
    ⇒ Index (AddressIndexDerivationType Shelley) PaymentK
    → AddressWithKey
  makePaymentAddressWithKey paymentAddrIx =
    AddressWithKey
      { cardanoAddress
      , ledgerAddress =
          fromMaybe
            (error "Can't convert Cardano.Address.Address to Ledger Addr")
            (toLedgerAddress cardanoAddress)
      , key = paymentKey
      }
   where
    credential ∷ Credential PaymentK =
      PaymentFromExtendedKey (toXPub <$> paymentKey)
    paymentKey ∷ Shelley PaymentK XPrv =
      deriveAddressPrivateKey deriveShelleyAccountKey role paymentAddrIx
     where
      deriveShelleyAccountKey ∷ Shelley AccountK XPrv =
        deriveAccountPrivateKey masterKey accountIx
       where
        accountIx ∷ Index 'Hardened 'AccountK = minBound
        masterKey = genMasterKeyFromMnemonic (SomeMnemonic mnemonic) mempty
    cardanoAddress ∷ Address =
      paymentAddress networkTag credential

toLedgerAddress ∷ Address → Maybe LedgerAddress
toLedgerAddress address = Ledger.decodeAddr (unAddress address)
