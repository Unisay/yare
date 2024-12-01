{-# LANGUAGE UndecidableInstances #-}

module Yare.App.Scripts.Test
  ( script
  , serialisedScript
  , serialisedScriptHash
  , blueprint
  ) where

import Plutus.Prelude qualified as P
import Relude

import Cardano.Api.Shelley
  ( PlutusScript (..)
  , PlutusScriptV3
  , PlutusScriptVersion (PlutusScriptV3)
  , Script (PlutusScript)
  )
import Data.ByteString.Short qualified as BS
import Data.Set qualified as Set
import PlutusLedgerApi.Common (serialiseCompiledCode, unsafeFromBuiltinData)
import PlutusTx.Blueprint

type Redeemer = Integer

script ∷ Script PlutusScriptV3
script = PlutusScript PlutusScriptV3 (PlutusScriptSerialised serialisedScript)

blueprint ∷ ContractBlueprint
blueprint =
  MkContractBlueprint
    { contractId = Just "Yare"
    , contractPreamble =
        MkPreamble
          { preambleTitle = "Yare"
          , preambleDescription = Nothing
          , preambleVersion = "0.1.0"
          , preamblePlutusVersion = PlutusV3
          , preambleLicense = Nothing
          }
    , contractValidators =
        Set.fromList
          [ MkValidatorBlueprint
              { validatorTitle = "Always succeeds"
              , validatorDescription = Nothing
              , validatorRedeemer =
                  MkArgumentBlueprint
                    { argumentTitle = Just "Redeemer"
                    , argumentDescription = Nothing
                    , argumentPurpose = Set.singleton Spend
                    , argumentSchema = definitionRef @Redeemer
                    }
              , validatorDatum = Nothing
              , validatorParameters = []
              , validatorCompiled = Just validatorCompiled'
              }
          ]
    , contractDefinitions = deriveDefinitions @'[Redeemer]
    }

serialisedScript ∷ P.SerialisedScript
serialisedScript = serialiseCompiledCode compiledCode

serialisedScriptHash ∷ ByteString
serialisedScriptHash = compiledValidatorHash validatorCompiled'

validatorCompiled' ∷ CompiledValidator
validatorCompiled' = compiledValidator PlutusV3 (BS.fromShort serialisedScript)

compiledCode ∷ P.CompiledCode (P.BuiltinData → P.BuiltinUnit)
compiledCode = $$(P.compile [||validator||])

{-# INLINEABLE validator #-}
validator ∷ P.BuiltinData → P.BuiltinUnit
validator scriptContextData =
  P.check (redeemer P.== 42)
 where
  redeemer ∷ Redeemer =
    unsafeFromBuiltinData
      (P.getRedeemer (P.scriptContextRedeemer scriptContext))
  scriptContext ∷ P.ScriptContext =
    unsafeFromBuiltinData scriptContextData
