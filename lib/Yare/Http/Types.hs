module Yare.Http.Types
  ( module Address
  , Utxo (..)
  , ChainTip (..)
  , Script (..)
  , ScriptHash (..)
  , ScriptStatus (..)
  ) where

import Yare.Prelude

import Cardano.Api qualified as CA
import Cardano.Api.Shelley (renderTxIn)
import Data.Aeson (ToJSON (..), object, pairs, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding (pairStr)
import Data.Aeson.Encoding qualified as Json
import Data.ByteString.Base16 qualified as Base16
import Data.Map.Strict qualified as Map
import GHC.Exts (IsList (toList))
import Ouroboros.Network.Block qualified as NB
import Servant
  ( FromHttpApiData (..)
  , MimeRender (mimeRender)
  , MimeUnrender (..)
  , PlainText
  , ToHttpApiData (toUrlPiece)
  )
import Yare.App.Services.DeployScript qualified as DeployScript
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

--------------------------------------------------------------------------------
-- Script ----------------------------------------------------------------------

newtype Script = Script {script ∷ ByteString}
  deriving stock (Eq, Show, Generic)

instance MimeUnrender PlainText Script where
  mimeUnrender _proxy = Right . Script . toStrict

--------------------------------------------------------------------------------
-- Script Hash -----------------------------------------------------------------

newtype ScriptHash = ScriptHash {scriptHash ∷ ByteString}
  deriving stock (Eq, Show, Generic)

instance ToHttpApiData ScriptHash where
  toUrlPiece (ScriptHash hash) = decodeUtf8 (Base16.encode hash)

instance FromHttpApiData ScriptHash where
  parseUrlPiece src =
    fmap ScriptHash . first toText . Base16.decode $ encodeUtf8 src

--------------------------------------------------------------------------------
-- Script Status ---------------------------------------------------------------

newtype ScriptStatus = ScriptStatus DeployScript.ScriptStatus
  deriving stock (Eq, Show, Generic)

instance MimeRender PlainText ScriptStatus where
  mimeRender _proxy (ScriptStatus status) =
    case status of
      DeployScript.ScriptStatusUnknown →
        "unknown"
      DeployScript.ScriptStatusDeployInitiated txIn →
        "deploy-initiated(" <> encodeUtf8 (renderTxIn txIn) <> ")"
      DeployScript.ScriptStatusDeployCompleted txIn →
        "deploy-completed(" <> encodeUtf8 (renderTxIn txIn) <> ")"

instance ToJSON ScriptStatus where
  toJSON (ScriptStatus status) =
    case status of
      DeployScript.ScriptStatusUnknown →
        object ["status" .= ("unknown" ∷ Text)]
      DeployScript.ScriptStatusDeployInitiated i →
        object ["txIn" .= i, "status" .= ("deploy-initiated" ∷ Text)]
      DeployScript.ScriptStatusDeployCompleted i →
        object ["txIn" .= i, "status" .= ("deploy-completed" ∷ Text)]
