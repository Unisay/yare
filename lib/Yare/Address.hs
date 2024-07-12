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
import Yare.Chain.Types (LedgerAddress)

type AddressWithKey ∷ Type
data AddressWithKey = AddressWithKey
  { address ∷ Address
  , key ∷ Shelley PaymentK XPrv
  }

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
    ∷ Index (AddressIndexDerivationType Shelley) PaymentK
    → AddressWithKey
  makePaymentAddressWithKey paymentAddrIx =
    AddressWithKey
      { address = paymentAddress networkTag credential
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

toLedgerAddress ∷ Address → Maybe LedgerAddress
toLedgerAddress address = Ledger.decodeAddr (unAddress address)
