module Yare.Http.Types
  ( module Address
  , Utxo (..)
  , ChainTip (..)
  ) where

import Yare.Prelude

import Cardano.Api qualified as CA
import Data.Aeson (ToJSON (..), object, pairs, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding (pairStr)
import Data.Aeson.Encoding qualified as Json
import Data.Map.Strict qualified as Map
import GHC.Exts (IsList (toList))
import Ouroboros.Network.Block qualified as NB
import Yare.Chain.Types (ledgerAddressToText)
import Yare.Chain.Types qualified as Y
import Yare.Http.Address as Address
import Yare.Utxo qualified as Y

--------------------------------------------------------------------------------
-- UTXO ------------------------------------------------------------------------

newtype Utxo = Utxo Y.Entries
  deriving stock (Eq, Show)

instance ToJSON Utxo where
  toJSON (Utxo utxo) = toJSON do
    Map.toList utxo <&> \(txIn, (addr, value)) →
      object
        [ "txIn" .= txIn
        , "address" .= toJSON (ledgerAddressToText addr)
        , "value" .= map (bimap assetIdToJSON toJSON) (GHC.Exts.toList value)
        ]
   where
    assetIdToJSON ∷ CA.AssetId → Aeson.Value
    assetIdToJSON = \case
      CA.AdaAssetId → Aeson.String "lovelace"
      CA.AssetId policyId assetName → toJSON (policyId, assetName)

--------------------------------------------------------------------------------
-- Chain Tip -------------------------------------------------------------------

newtype ChainTip = ChainTip Y.ChainTip
  deriving stock (Eq, Show)

instance ToJSON ChainTip where
  toJSON (ChainTip tip) = toJSON do
    case tip of
      NB.TipGenesis → "genesis"
      NB.Tip slotNo hash blockNo →
        object
          [ "slotNo" .= slotNo
          , "headerHash" .= show @String hash
          , "blockNo" .= blockNo
          ]
  toEncoding (ChainTip tip) =
    case tip of
      NB.TipGenesis → Json.text "genesis"
      NB.Tip slotNo hash blockNo →
        pairs $
          pairStr "slotNo" (toEncoding slotNo)
            <> pairStr "headerHash" (Json.text (show hash))
            <> pairStr "blockNo" (toEncoding blockNo)
