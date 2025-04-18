module Main (main) where

import Yare.Prelude

import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short qualified as SBS
import Data.Text.IO qualified as Text
import Main.Utf8 (withUtf8)
import PlutusLedgerApi.Common (uncheckedDeserialiseUPLC)
import PlutusTx.Blueprint
import Text.PrettyBy.Default (display)
import Yare.App.Scripts (YareScript (..), testYareScript)

main ∷ IO ()
main = withUtf8 do
  let blueprintFile ∷ FilePath = "dist/blueprint.json"
  writeBlueprint blueprintFile yareScriptBlueprint
  putStrLn $ "Wrote blueprint to " <> blueprintFile

  let validatorFile ∷ FilePath = "dist/validator.uplc.flat.cbor"
  BS.writeFile validatorFile (SBS.fromShort yareScriptSerialised)
  putStrLn $ "Wrote validator to " <> validatorFile

  let validatorFileHex ∷ FilePath = "dist/validator.uplc.flat.cbor.hex"
  BS.writeFile
    validatorFileHex
    (Base16.encode (SBS.fromShort yareScriptSerialised))
  putStrLn $ "Wrote validator hex to " <> validatorFileHex

  let validatorHashFile ∷ FilePath = "dist/validator.hash"
  BS.writeFile validatorHashFile yareScriptHash
  putStrLn $ "Wrote validator hash to " <> validatorHashFile

  let validatorHashFileHex ∷ FilePath = "dist/validator.hash.hex"
  BS.writeFile validatorHashFileHex (Base16.encode yareScriptHash)
  putStrLn $ "Wrote validator hash hex to " <> validatorHashFileHex

  let uplc = uncheckedDeserialiseUPLC yareScriptSerialised
  let validatorFileUplc ∷ FilePath = "dist/validator.uplc"
  Text.writeFile validatorFileUplc (display uplc)
  putStrLn $ "Wrote validator UPLC to " <> validatorFileUplc
 where
  YareScript
    { yareScriptHash
    , yareScriptSerialised
    , yareScriptBlueprint
    } = testYareScript
