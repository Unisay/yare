{-# OPTIONS_GHC -Wno-orphans #-}

module Fmt.Orphans () where

import Yare.Prelude

import Cardano.Api.Ledger (Coin (..))
import Cardano.Api.Shelley
  ( AssetId (..)
  , AssetName
  , PolicyId (..)
  , Quantity (..)
  , ScriptHash
  , TxId
  , TxIn
  , TxIx (..)
  , Value
  , renderTxIn
  , selectLovelace
  , serialiseToRawBytesHexText
  )
import Cardano.Ledger.Api (StandardCrypto)
import Cardano.Ledger.Credential (PaymentCredential, credToText)
import Cardano.Slotting.Slot (WithOrigin, withOrigin)
import Fmt (Buildable (..), nameF, (+|))
import Fmt.Internal.Numeric (groupInt)
import GHC.Exts qualified
import Ouroboros.Network.Block
  ( BlockNo (unBlockNo)
  , Point
  , SlotNo (unSlotNo)
  , pointSlot
  )
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Types (LedgerAddress, ledgerAddressToText)

instance Buildable TxIn where
  build = build . renderTxIn

instance Buildable TxId where
  build = build . serialiseToRawBytesHexText

deriving newtype instance Buildable TxIx

instance Buildable Coin where
  build = build . unCoin

instance Buildable Value where
  build value =
    groupInt 3 '_' (selectLovelace value)
      <> " Lovelace"
        +| if length (GHC.Exts.toList value) > 1
          then " and other assets"
          else ""

instance Buildable AssetId where
  build AdaAssetId = "Lovelace"
  build (AssetId policyId assetName) =
    nameF "AssetId" do
      nameF "Policy ID" (build policyId)
        <> nameF "Asset Name" (build assetName)

instance Buildable PolicyId where
  build (PolicyId policyId) = nameF "PolicyId" (build policyId)

instance Buildable ScriptHash where
  build = nameF "Script Hash" . build . serialiseToRawBytesHexText

instance Buildable AssetName where
  build assetName = nameF "Asset Name" (build assetName)

instance Buildable Quantity where
  build (Quantity quantity) = build quantity

instance Buildable (WithOrigin SlotNo) where
  build = withOrigin (build (0 ∷ Int)) build

instance Buildable SlotNo where
  build = groupInt 3 '_' . unSlotNo

instance Buildable BlockNo where
  build = groupInt 3 '_' . unBlockNo

instance Buildable (Point StdCardanoBlock) where
  build point = nameF "Chain Point" (build (pointSlot point))

instance Buildable LedgerAddress where
  build = build . ledgerAddressToText

instance Buildable (PaymentCredential StandardCrypto) where
  build = build . credToText
