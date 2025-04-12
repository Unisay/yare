module Yare.Util.Tx.Construction
  ( mkScriptOutput
  , minAdaValue
  , mkCardanoApiUtxo
  , witnessUtxoEntry
  ) where

import Yare.Prelude

import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Shelley
  ( BabbageEraOnwards
  , Convert (convert)
  , ConwayEraOnwards
  , CtxTx
  , LedgerProtocolParameters (unLedgerProtocolParameters)
  , Lovelace
  , MaryEraOnwards
  , ReferenceScript (ReferenceScriptNone)
  , ShelleyBasedEra
  , ShelleyLedgerEra
  , ShelleyWitnessSigningKey (WitnessPaymentExtendedKey)
  , SigningKey (PaymentExtendedSigningKey)
  , TxOut (..)
  , TxOutDatum (TxOutDatumNone)
  , TxOutValue (TxOutValueShelleyBased)
  , UTxO (UTxO)
  , calculateMinimumUTxO
  , fromShelleyAddr
  , fromShelleyAddrIsSbe
  , shelleyBasedEraConstraints
  , toLedgerValue
  )
import Cardano.Ledger.Val (modifyCoin)
import Data.Map.Strict qualified as Map
import Yare.Chain.Types (LedgerAddress)
import Yare.Utxo (Entry (MkEntry))
import Yare.Utxo qualified as Utxo

{- | Construct a transaction output with a datum and a reference script.
The value of the output is calculated to be the minimum UTxO value.
-}
mkScriptOutput
  ∷ ShelleyBasedEra era
  → LedgerProtocolParameters era
  → LedgerAddress
  → Ledger.Value (ShelleyLedgerEra era)
  → TxOutDatum CtxTx era
  → ReferenceScript era
  → (Lovelace, TxOut CtxTx era)
mkScriptOutput era protocolParameters addr value datum script =
  let ada = minAdaValue era protocolParameters addr value datum script
      val =
        shelleyBasedEraConstraints era $
          TxOutValueShelleyBased era (modifyCoin (+ ada) value)
   in (ada, TxOut (fromShelleyAddrIsSbe era addr) val datum script)
{-# INLINEABLE mkScriptOutput #-}

-- | Calculate the minimum ADA value for a transaction output.
minAdaValue
  ∷ ShelleyBasedEra era
  → LedgerProtocolParameters era
  → LedgerAddress
  → Ledger.Value (ShelleyLedgerEra era)
  → TxOutDatum CtxTx era
  → ReferenceScript era
  → Lovelace
minAdaValue era protocolParameters addr value datum refScript =
  calculateMinimumUTxO
    era
    ( TxOut
        (fromShelleyAddrIsSbe era addr)
        (shelleyBasedEraConstraints era (TxOutValueShelleyBased era value))
        datum
        refScript
    )
    (unLedgerProtocolParameters protocolParameters)
{-# INLINEABLE minAdaValue #-}

{- | Convert a list of inputs with their corresponding addresses and values
 to a Cardano.Api.UTxO to use in the transaction construction functions.
-}
mkCardanoApiUtxo
  ∷ ∀ era f
   . Foldable f
  ⇒ ConwayEraOnwards era
  → f Utxo.Entry
  → UTxO era
mkCardanoApiUtxo conwayEraOnwards =
  UTxO . Map.fromList . map toTxOut . toList
 where
  shelleyBasedEra ∷ ShelleyBasedEra era =
    convert conwayEraOnwards
  babbageEraOnwards ∷ BabbageEraOnwards era =
    convert conwayEraOnwards
  maryEraOnwards ∷ MaryEraOnwards era =
    convert babbageEraOnwards
  toTxOut MkEntry {..} =
    ( utxoEntryInput
    , TxOut
        (fromShelleyAddr shelleyBasedEra utxoEntryAddress)
        ( shelleyBasedEraConstraints shelleyBasedEra $
            TxOutValueShelleyBased shelleyBasedEra $
              toLedgerValue maryEraOnwards utxoEntryValue
        )
        TxOutDatumNone
        ReferenceScriptNone
    )

witnessUtxoEntry ∷ Utxo.Entry → ShelleyWitnessSigningKey
witnessUtxoEntry =
  WitnessPaymentExtendedKey
    . PaymentExtendedSigningKey
    . Utxo.utxoEntryKey
