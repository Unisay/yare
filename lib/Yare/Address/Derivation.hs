{-# OPTIONS_GHC -Wno-orphans #-}

module Yare.Address.Derivation
  ( externalPaymentAdressesKeys
  , internalPaymentAdressesKeys
  , AddressWithKey (..)
  , toLedgerAddress
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
import Cardano.Api.Shelley qualified as CApi
import Cardano.Ledger.Api.Tx.Address qualified as Ledger
import Cardano.Mnemonic (Mnemonic, SomeMnemonic (..))
import Data.Traversable (for)
import NoThunks.Class (InspectHeap (..))
import NoThunks.Class.Extended (NoThunks)
import Text.Show (show)
import Yare.Chain.Types (LedgerAddress)

type AddressWithKey ∷ Type
data AddressWithKey = AddressWithKey
  { cardanoAddress ∷ !Address
  , ledgerAddress ∷ !LedgerAddress
  , witness ∷ !CApi.ShelleyWitnessSigningKey
  }
  deriving stock (Generic)
  deriving anyclass (NoThunks, NFData)

-- Orphan instance
instance NFData CApi.ShelleyWitnessSigningKey where
  rnf = \case
    CApi.WitnessPaymentKey
      (CApi.PaymentSigningKey !_PaymentKey) → ()
    CApi.WitnessPaymentExtendedKey
      (CApi.PaymentExtendedSigningKey !_PaymentExtendedKey) → ()
    CApi.WitnessStakeKey
      (CApi.StakeSigningKey !_StakeKey) → ()
    CApi.WitnessStakeExtendedKey
      (CApi.StakeExtendedSigningKey !_StakeExtendedKey) → ()
    CApi.WitnessStakePoolKey
      (CApi.StakePoolSigningKey !_StakePoolKey) → ()
    CApi.WitnessGenesisKey
      (CApi.GenesisSigningKey !_GenesisKey) → ()
    CApi.WitnessGenesisExtendedKey
      (CApi.GenesisExtendedSigningKey !_GenesisExtendedKey) → ()
    CApi.WitnessGenesisDelegateKey
      (CApi.GenesisDelegateSigningKey !_GenesisDelegateKey) → ()
    CApi.WitnessGenesisDelegateExtendedKey
      (CApi.GenesisDelegateExtendedSigningKey !_GenesisDelegateExtendedKey) → ()
    CApi.WitnessGenesisUTxOKey
      (CApi.GenesisUTxOSigningKey !_GenesisUTxOKey) → ()
    CApi.WitnessCommitteeColdKey
      (CApi.CommitteeColdSigningKey !_CommitteeColdKey) → ()
    CApi.WitnessCommitteeColdExtendedKey
      (CApi.CommitteeColdExtendedSigningKey !_CommitteeColdExtendedKey) → ()
    CApi.WitnessCommitteeHotKey
      (CApi.CommitteeHotSigningKey !_CommitteeHotKey) → ()
    CApi.WitnessCommitteeHotExtendedKey
      (CApi.CommitteeHotExtendedSigningKey !_CommitteeHotExtendedKey) → ()
    CApi.WitnessDRepKey
      (CApi.DRepSigningKey !_DRepKey) → ()
    CApi.WitnessDRepExtendedKey
      (CApi.DRepExtendedSigningKey !_DRepExtendedKey) → ()

-- Orphan instance
deriving via
  InspectHeap CApi.ShelleyWitnessSigningKey
  instance
    NoThunks CApi.ShelleyWitnessSigningKey

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
    let witness =
          CApi.WitnessPaymentExtendedKey $
            CApi.PaymentExtendedSigningKey $
              CAddr.getKey paymentKey
    ledgerAddress ← toLedgerAddress cardanoAddress
    pure AddressWithKey {cardanoAddress, ledgerAddress, witness = witness}
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

--------------------------------------------------------------------------------
-- Errors ----------------------------------------------------------------------

type Error ∷ Type
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
