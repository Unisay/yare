{-# OPTIONS_GHC -Wno-orphans #-}

module Fmt.Orphans () where

import Relude

import Cardano.Api.Shelley
  ( AssetId (..)
  , AssetName
  , PolicyId (..)
  , Quantity (..)
  , ScriptHash
  , TxIn
  , Value
  , renderTxIn
  )
import Cardano.Slotting.Slot (SlotNo (unSlotNo), WithOrigin, withOrigin)
import Fmt (Buildable (..), blockListF, nameF, (+|))
import Fmt.Internal.Numeric (groupInt)
import GHC.Exts qualified

instance Buildable TxIn where
  build = build . renderTxIn

instance Buildable Value where
  build value =
    case GHC.Exts.toList value of
      [(AdaAssetId, quantity)] → build quantity +| " Lovelace"
      vs →
        blockListF
          [nameF (build asset) (build quantity) | (asset, quantity) ← vs]

instance Buildable AssetId where
  build AdaAssetId = "Lovelace"
  build (AssetId policyId assetName) =
    nameF "AssetId" do
      nameF "Policy ID" (build policyId)
        <> nameF "Asset Name" (build assetName)

instance Buildable PolicyId where
  build (PolicyId policyId) = nameF "PolicyId" (build policyId)

instance Buildable ScriptHash where
  build scriptHash = nameF "Script Hash" (build scriptHash)

instance Buildable AssetName where
  build assetName = nameF "Asset Name" (build assetName)

instance Buildable Quantity where
  build (Quantity quantity) = build quantity

instance Buildable (WithOrigin SlotNo) where
  build = withOrigin (build (0 ∷ Int)) (groupInt 3 '_' . unSlotNo)
