module Yare.App.Scripts.MintingPolicy
  ( MintingParams (..)
  , serialised
  ) where

import Plutus.Prelude

import Cardano.Api.Shelley qualified as CA
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import PlutusTx qualified

data MintingParams = MkMintingParams
  { whoCanMint ∷ PubKeyHash
  , singletonTxOut ∷ TxOutRef
  }
  deriving stock (Generic)

$(PlutusTx.makeLift ''MintingParams)

type MintingRedeemer = ()

{-# INLINEABLE policy #-}
policy ∷ MintingParams → ScriptContext → Bool
policy MkMintingParams {whoCanMint, singletonTxOut} scriptContext =
  mintedExactlyOneToken && signedBy whoCanMint && ensureSpentOutput
 where
  txInfo = scriptContextTxInfo scriptContext
  signedBy = txSignedBy txInfo
  mintedExactlyOneToken =
    case flattenValue (mintValueMinted (txInfoMint txInfo)) of
      [(currency, _tokenName, quantity)] →
        currency == ownCurrencySymbol scriptContext && quantity == 1
      _ → False
  ensureSpentOutput =
    case findTxInByTxOutRef singletonTxOut txInfo of
      Nothing → False
      Just _i → True

--------------------------------------------------------------------------------
-- Wrap and compile the policy -------------------------------------------------

untyped ∷ MintingParams → BuiltinData → BuiltinUnit
untyped params scriptContext =
  check (policy params (unsafeFromBuiltinData scriptContext))

compiled ∷ MintingParams → CompiledCode (BuiltinData → BuiltinUnit)
compiled params =
  $$(compile [||untyped||]) `unsafeApplyCode` liftCodeDef params

type SerialisedScriptHash = ByteString

serialised ∷ MintingParams → (CA.PlutusScript CA.PlutusScriptV3, CA.ScriptHash)
serialised params = (apiScript, apiScriptHash)
 where
  code = compiled params
  apiScript = CA.PlutusScriptSerialised (serialiseCompiledCode code)
  apiScriptHash = CA.hashScript (CA.PlutusScript CA.PlutusScriptV3 apiScript)
