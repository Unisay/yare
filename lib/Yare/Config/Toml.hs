module Yare.Config.Toml (Config (..), readConfig, readConfigFrom) where

import Relude

import Control.Exception (throw)
import Data.ByteString.Char8 (pack)
import Network.Wai.Handler.Warp (Port)
import TOML.Decode (DecodeTOML (..), Decoder, decodeFile, getFields)

-- | Data that must be present in `config.toml`
type Config ∷ Type
data Config = Config
  { apiHttpPort ∷ Port
  , txId ∷ ByteString
  }

instance DecodeTOML Config where
  tomlDecoder =
    Config
      <$> portField
      <*> txIdField
   where
    txIdField ∷ Decoder ByteString
    txIdField = getFields ["transaction", "txId"] <&> pack

    portField ∷ Decoder Port
    portField = getFields ["server", "apiHttpPort"]

readConfigFrom ∷ FilePath → IO Config
readConfigFrom pathToConfig = decodeFile pathToConfig >>= either throw pure

readConfig ∷ IO Config
readConfig = readConfigFrom "./config.toml"
