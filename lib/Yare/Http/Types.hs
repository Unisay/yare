module Yare.Http.Types
  ( Utxo (..)
  ) where

import Relude

import Cardano.Address qualified as CA
import Cardano.Ledger.Address qualified as L
import Data.Aeson (ToJSON (..), object, pairs, (.=))
import Data.Aeson.Encoding (pairStr)
import Data.Aeson.Encoding qualified as Json
import Data.Map.Strict qualified as Map
import Yare.Chain.Types (LedgerAddress)
import Yare.Utxo qualified as Y

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
