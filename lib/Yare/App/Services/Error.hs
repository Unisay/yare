module Yare.App.Services.Error where

import Yare.Prelude hiding (show)

import Cardano.Api.Shelley
  ( InAnyShelleyBasedEra (..)
  , TxBodyErrorAutoBalance
  )
import Text.Show (show)

data TxConstructionError
  = NoFeeInputs
  | NoCollateralInputs
  | TxAutoBalanceError (InAnyShelleyBasedEra TxBodyErrorAutoBalance)

instance Show TxConstructionError where
  show = \case
    NoFeeInputs →
      "No fee inputs available."
    NoCollateralInputs →
      "No collateral inputs available."
    TxAutoBalanceError (InAnyShelleyBasedEra era e) →
      "Tx balancing error in era " <> show era <> ": " <> show e

