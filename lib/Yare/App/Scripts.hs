module Yare.App.Scripts where

import Yare.Prelude

import Cardano.Api
  ( PlutusScriptV3
  , Script
  , ScriptHash (..)
  , serialiseToRawBytes
  )
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Credential (credScriptHash)
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx.Blueprint (ContractBlueprint)
import Yare.App.Scripts.Test qualified as Test
import Yare.Chain.Types (LedgerAddress)

data YareScript = YareScript
  { yareScript ∷ Script PlutusScriptV3
  , yareScriptHash ∷ ByteString
  , yareScriptSerialised ∷ V3.SerialisedScript
  , yareScriptBlueprint ∷ ContractBlueprint
  }

testYareScript ∷ YareScript
testYareScript =
  YareScript
    { yareScript = Test.script
    , yareScriptHash = Test.serialisedScriptHash
    , yareScriptSerialised = Test.serialisedScript
    , yareScriptBlueprint = Test.blueprint
    }

yareScripts ∷ [YareScript]
yareScripts = [testYareScript]

yareScriptHashes ∷ [ByteString]
yareScriptHashes = map yareScriptHash yareScripts

isOwnScriptAddress ∷ LedgerAddress → Maybe ScriptHash
isOwnScriptAddress = \case
  Addr _network cred _stakeCred
    | Just scriptHash ← credScriptHash cred → do
        let apiScriptHash = ScriptHash scriptHash
        guard (serialiseToRawBytes apiScriptHash `elem` yareScriptHashes)
        pure apiScriptHash
  _ → Nothing
