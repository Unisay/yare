module Yare.Http.Types
  ( module Address
  , Utxo (..)
  , NetworkInfo (..)
  , ChainTip (..)
  , BlockRef (..)
  , Script (..)
  , ScriptDeployment (..)
  , Transactions (..)
  ) where

import Yare.Prelude

import Cardano.Api qualified as CA
import Cardano.Api.Shelley (ScriptHash, TxId, TxIn (..))
import Data.Aeson (ToJSON (..), object, pairs, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding (pairStr)
import Data.Aeson.Encoding qualified as Json
import Data.Map.Strict qualified as Map
import GHC.Exts (IsList (toList))
import Ouroboros.Network.Block qualified as NB
import Servant (MimeUnrender (..), PlainText)
import Yare.App.Services.DeployScript qualified as DeployScript
import Yare.Chain.Types (ledgerAddressToText)
import Yare.Chain.Types qualified as Y
import Yare.Http.Address as Address
import Yare.Utxo qualified as DeployScript
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
-- Network Info ----------------------------------------------------------------

data NetworkInfo = NetworkInfo
  { networkTip ∷ ChainTip
  , lastIndexed ∷ Maybe BlockRef
  }

deriving stock instance Generic NetworkInfo
deriving anyclass instance ToJSON NetworkInfo

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
-- Block Reference -------------------------------------------------------------

newtype BlockRef = BlockRef Y.BlockRef
  deriving stock (Eq, Show)

instance ToJSON BlockRef where
  toJSON (BlockRef (Y.BlockRef {..})) = toJSON do
    object
      [ "slotNo" .= blockRefSlot
      , "blockNo" .= blockRefBlock
      , "headerHash" .= show @String blockRefHash
      ]
  toEncoding (BlockRef (Y.BlockRef {..})) =
    pairs $
      pairStr "slotNo" (toEncoding blockRefSlot)
        <> pairStr "blockNo" (toEncoding blockRefBlock)
        <> pairStr "headerHash" (Json.text (show blockRefHash))

--------------------------------------------------------------------------------
-- Script ----------------------------------------------------------------------

newtype Script = Script {script ∷ ByteString}
  deriving stock (Eq, Show, Generic)

instance MimeUnrender PlainText Script where
  mimeUnrender _proxy = Right . Script . toStrict

--------------------------------------------------------------------------------
-- Script Status ---------------------------------------------------------------

data ScriptDeployment
  = ScriptDeployment ScriptHash DeployScript.ScriptDeployment
  deriving stock (Eq, Show, Generic)

instance ToJSON ScriptDeployment where
  toJSON (ScriptDeployment scriptHash DeployScript.ScriptDeployment {..}) =
    object
      [ "scriptHash" .= scriptHash
      , "scriptStatus" .= renderScriptStatus scriptStatus
      , "scriptTxId" .= txId
      , "scriptTxOut" .= txIx
      ]
   where
    TxIn txId txIx = scriptTxIn

renderScriptStatus ∷ DeployScript.ScriptStatus → Text
renderScriptStatus = \case
  DeployScript.ScriptStatusDeployInitiated → "deploy-initiated"
  DeployScript.ScriptStatusDeployCompleted → "deploy-completed"

--------------------------------------------------------------------------------
-- Transactions ----------------------------------------------------------------

data Transactions = Transactions
  { submitted ∷ Set TxId
  , inLedger ∷ Set TxId
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Transactions where
  toJSON Transactions {..} =
    object
      [ "submitted" .= submitted
      , "inLedger" .= inLedger
      ]
  toEncoding Transactions {..} =
    pairs $
      pairStr "submitted" (toEncoding submitted)
        <> pairStr "inLedger" (toEncoding inLedger)
