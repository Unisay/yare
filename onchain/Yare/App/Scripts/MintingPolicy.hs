module Yare.App.Scripts.MintingPolicy
  ( MintingParams
  , serialised
  ) where

import Plutus.Prelude

type MintingParams = (PubKeyHash, TxOutRef)

type MintingRedeemer = ()

{-# INLINEABLE policy #-}
policy ∷ MintingParams → ScriptContext → Bool
policy (whoCanMint, txOutRef) scriptContext =
  signedBy whoCanMint
    && mintedExactlyOneToken
    && ensureSpentOutput
 where
  txInfo = scriptContextTxInfo scriptContext
  signedBy = txSignedBy txInfo
  mintedExactlyOneToken =
    case flattenValue (txInfoMint txInfo) of
      [(currency, _tokenName, quantity)] →
        currency == ownCurrencySymbol scriptContext && quantity == 1
      _ → False
  ensureSpentOutput =
    case findTxInByTxOutRef txOutRef txInfo of
      Nothing → False
      Just _i → True

--------------------------------------------------------------------------------
-- Wrap and compile the policy -------------------------------------------------

untyped ∷ MintingParams → BuiltinData → BuiltinUnit
untyped params scriptContext =
  check (policy params (unsafeFromBuiltinData scriptContext))

compiled ∷ MintingParams → CompiledCode (BuiltinData → BuiltinUnit)
compiled params =
  $$(compile [||untyped||]) `unsafeApplyCode` liftCode plcVersion110 params

serialised ∷ MintingParams → SerialisedScript
serialised = serialiseCompiledCode . compiled
