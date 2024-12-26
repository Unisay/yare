module Yare.Util.Tx.Construction
  ( mkScriptOutput
  , mkUtxoFromInputs
  ) where

import Yare.Prelude

import Cardano.Api.Shelley qualified as A
import Data.Map.Strict qualified as Map
import Yare.Address (AddressWithKey (..))
import Yare.Chain.Types (LedgerAddress)

{- | Construct a transaction output with a datum and a reference script.
The value of the output is calculated to be the minimum UTxO value.
-}
mkScriptOutput
  ∷ A.ShelleyBasedEra era
  → A.LedgerProtocolParameters era
  → LedgerAddress
  → A.TxOutDatum A.CtxTx era
  → A.ReferenceScript era
  → A.TxOut A.CtxTx era
mkScriptOutput era protocolParameters addr datum refScript =
  A.TxOut eraAddress minAdaValue datum refScript
 where
  eraAddress = A.fromShelleyAddrIsSbe era addr
  emptyValue = A.lovelaceToTxOutValue era 0
  minAdaValue =
    A.lovelaceToTxOutValue era $
      A.calculateMinimumUTxO
        era
        (A.TxOut eraAddress emptyValue datum refScript)
        (A.unLedgerProtocolParameters protocolParameters)

{- | Convert a list of inputs with their corresponding addresses and values
 to a Cardano.Api.UTxO to use in the transaction construction functions.
-}
mkUtxoFromInputs
  ∷ ∀ era f
   . Foldable f
  ⇒ A.ConwayEraOnwards era
  → f (A.TxIn, (AddressWithKey, A.Value))
  → A.UTxO era
mkUtxoFromInputs conwayEraOnwards inputs =
  A.UTxO $ Map.fromList $ toTxOut <<$>> toList inputs
 where
  shelleyBasedEra ∷ A.ShelleyBasedEra era = A.inject conwayEraOnwards
  babbageEraOnwards ∷ A.BabbageEraOnwards era = A.inject conwayEraOnwards
  maryEraOnwards ∷ A.MaryEraOnwards era = A.inject babbageEraOnwards
  toTxOut (AddressWithKey {ledgerAddress}, value) =
    A.TxOut
      (A.fromShelleyAddr shelleyBasedEra ledgerAddress)
      ( A.shelleyBasedEraConstraints shelleyBasedEra $
          A.TxOutValueShelleyBased shelleyBasedEra $
            A.toLedgerValue maryEraOnwards value
      )
      A.TxOutDatumNone
      A.ReferenceScriptNone
