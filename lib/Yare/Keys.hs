module Yare.Keys (genMnemonic, paymentAdressesWithKeys) where

import Relude

import Cardano.Address (Address, NetworkTag (..))
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
import Cardano.Mnemonic
  ( Mnemonic
  , SomeMnemonic (..)
  , entropyToMnemonic
  , genEntropy
  )

genMnemonic ∷ IO (Mnemonic 24)
genMnemonic = entropyToMnemonic <$> genEntropy @256

data AddressWithKey = AddressWithKey
  { address ∷ Address
  , key ∷ Shelley PaymentK XPrv
  }

paymentAdressesWithKeys ∷ NetworkTag → Mnemonic 24 → [AddressWithKey]
paymentAdressesWithKeys networkTag mnemonic =
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
      deriveAddressPrivateKey deriveShelleyAccountKey UTxOExternal paymentAddrIx
     where
      deriveShelleyAccountKey ∷ Shelley AccountK XPrv =
        deriveAccountPrivateKey masterKey accountIx
       where
        accountIx ∷ Index 'Hardened 'AccountK = minBound
        masterKey = genMasterKeyFromMnemonic (SomeMnemonic mnemonic) mempty
