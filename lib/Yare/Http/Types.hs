module Yare.Http.Types
  ( Utxo (..)
  , ChainTip (..)
  ) where

import Relude

import Cardano.Address qualified as CA
import Cardano.Ledger.Address qualified as L
import Data.Aeson (ToJSON (..), object, pairs, (.=))
import Data.Aeson.Encoding (pairStr)
import Data.Aeson.Encoding qualified as Json
import Data.Map.Strict qualified as Map
import Ouroboros.Network.Block qualified as NB
import Yare.Chain.Types (LedgerAddress)
import Yare.Chain.Types qualified as Y
import Yare.Utxo qualified as Y

--------------------------------------------------------------------------------
-- UTXO ------------------------------------------------------------------------

newtype Utxo = Utxo Y.Utxo
  deriving stock (Eq, Show)

instance ToJSON Utxo where
  toJSON (Utxo utxo) = toJSON do
    Map.toList (Y.utxoEntries utxo) <&> \(txIn, (addr, value)) →
      object
        [ "txIn" .= txIn
        , "address" .= toJSON addr
        , "value" .= toJSON value
        ]
  toEncoding (Utxo utxo) =
    Json.list identity $
      Map.toList (Y.utxoEntries utxo) <&> \(txIn, (addr, value)) →
        pairs $
          pairStr "txIn" (toEncoding txIn)
            <> pairStr "address" (toEncoding (addressBech32 addr))
            <> pairStr "value" (toEncoding value)

addressBech32 ∷ LedgerAddress → Text
addressBech32 = CA.bech32 . CA.unsafeMkAddress . L.serialiseAddr

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
