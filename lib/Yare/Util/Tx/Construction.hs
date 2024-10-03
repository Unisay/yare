module Yare.Util.Tx.Construction
  ( mkScriptOutput
  , mkUtxoFromInputs
  ) where

import Yare.Prelude

import Cardano.Api qualified as CA
import Cardano.Api.Shelley
  ( CtxTx
  , LedgerProtocolParameters (unLedgerProtocolParameters)
  , ReferenceScript
  , ShelleyBasedEra
  , TxOut (..)
  , TxOutDatum
  )
import Cardano.Api.Shelley qualified as CA
import Data.Map.Strict qualified as Map
import Yare.Address (AddressWithKey (..))
import Yare.Chain.Types (LedgerAddress)

{- | Construct a transaction output with a datum and a reference script.
The value of the output is calculated to be the minimum UTxO value.
-}
mkScriptOutput
  ∷ ShelleyBasedEra era
  → LedgerProtocolParameters era
  → LedgerAddress
  → TxOutDatum CtxTx era
  → ReferenceScript era
  → TxOut CtxTx era
mkScriptOutput era protocolParameters addr datum refScript =
  TxOut eraAddress minAdaValue datum refScript
 where
  eraAddress = CA.fromShelleyAddrIsSbe era addr
  emptyValue = CA.lovelaceToTxOutValue era 0
  minAdaValue =
    CA.lovelaceToTxOutValue era $
      CA.calculateMinimumUTxO
        era
        (TxOut eraAddress emptyValue datum refScript)
        (unLedgerProtocolParameters protocolParameters)

{- | Convert a list of inputs with their corresponding addresses and values
 to a Cardano.Api.UTxO to use in the transaction construction functions.
-}
mkUtxoFromInputs
  ∷ Foldable f
  ⇒ CA.BabbageEraOnwards era
  → f (CA.TxIn, (AddressWithKey, CA.Value))
  → CA.UTxO era
mkUtxoFromInputs currentEra inputs =
  CA.UTxO $ Map.fromList $ toTxOut <<$>> toList inputs
 where
  shelleyBasedEra = CA.babbageEraOnwardsToShelleyBasedEra currentEra
  toTxOut (AddressWithKey {ledgerAddress}, value) =
    TxOut
      (CA.fromShelleyAddr shelleyBasedEra ledgerAddress)
      ( CA.shelleyBasedEraConstraints shelleyBasedEra $
          let maryEra = CA.babbageEraOnwardsToMaryEraOnwards currentEra
              ledgerValue = CA.toLedgerValue maryEra value
           in CA.TxOutValueShelleyBased shelleyBasedEra ledgerValue
      )
      CA.TxOutDatumNone
      CA.ReferenceScriptNone
