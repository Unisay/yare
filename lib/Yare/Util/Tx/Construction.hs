module Yare.Util.Tx.Construction
  ( mkScriptOutput
  , mkCardanoApiUtxo
  , witnessUtxoEntry
  ) where

import Yare.Prelude

import Cardano.Api.Shelley qualified as CA
import Data.Map.Strict qualified as Map
import Yare.Chain.Types (LedgerAddress)
import Yare.Utxo (Entry (MkEntry))
import Yare.Utxo qualified as Utxo

{- | Construct a transaction output with a datum and a reference script.
The value of the output is calculated to be the minimum UTxO value.
-}
mkScriptOutput
  ∷ CA.ShelleyBasedEra era
  → CA.LedgerProtocolParameters era
  → LedgerAddress
  → CA.TxOutDatum CA.CtxTx era
  → CA.ReferenceScript era
  → CA.TxOut CA.CtxTx era
mkScriptOutput era protocolParameters addr datum refScript =
  CA.TxOut eraAddress minAdaValue datum refScript
 where
  eraAddress = CA.fromShelleyAddrIsSbe era addr
  emptyValue = CA.lovelaceToTxOutValue era 0
  minAdaValue =
    CA.lovelaceToTxOutValue era $
      CA.calculateMinimumUTxO
        era
        (CA.TxOut eraAddress emptyValue datum refScript)
        (CA.unLedgerProtocolParameters protocolParameters)

{- | Convert a list of inputs with their corresponding addresses and values
 to a Cardano.Api.UTxO to use in the transaction construction functions.
-}
mkCardanoApiUtxo
  ∷ ∀ era f
   . Foldable f
  ⇒ CA.ConwayEraOnwards era
  → f Utxo.Entry
  → CA.UTxO era
mkCardanoApiUtxo conwayEraOnwards =
  CA.UTxO . Map.fromList . map toTxOut . toList
 where
  shelleyBasedEra ∷ CA.ShelleyBasedEra era =
    CA.convert conwayEraOnwards
  babbageEraOnwards ∷ CA.BabbageEraOnwards era =
    CA.convert conwayEraOnwards
  maryEraOnwards ∷ CA.MaryEraOnwards era =
    CA.convert babbageEraOnwards
  toTxOut MkEntry {..} =
    ( utxoEntryInput
    , CA.TxOut
        (CA.fromShelleyAddr shelleyBasedEra utxoEntryAddress)
        ( CA.shelleyBasedEraConstraints shelleyBasedEra $
            CA.TxOutValueShelleyBased shelleyBasedEra $
              CA.toLedgerValue maryEraOnwards utxoEntryValue
        )
        CA.TxOutDatumNone
        CA.ReferenceScriptNone
    )

witnessUtxoEntry ∷ Utxo.Entry → CA.ShelleyWitnessSigningKey
witnessUtxoEntry =
  CA.WitnessPaymentExtendedKey
    . CA.PaymentExtendedSigningKey
    . Utxo.utxoEntryKey
