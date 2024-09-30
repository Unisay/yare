{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

module Yare.App.Scripts
  ( script
  ) where

import Cardano.Api.Shelley
  ( PlutusScript (..)
  , PlutusScriptV3
  , PlutusScriptVersion (PlutusScriptV3)
  , Script (PlutusScript)
  )
import PlutusLedgerApi.Common (serialiseCompiledCode)
import PlutusTx (BuiltinData, CompiledCode, compile)
import PlutusTx.Prelude (Bool (True), BuiltinUnit, check)

script ∷ Script PlutusScriptV3
script =
  PlutusScript
    PlutusScriptV3
    (PlutusScriptSerialised (serialiseCompiledCode compiledCode))

compiledCode ∷ CompiledCode (BuiltinData → BuiltinUnit)
compiledCode = $$(compile [||validator||])

{-# INLINEABLE validator #-}
validator ∷ BuiltinData → BuiltinUnit
validator _scriptContext = check True
