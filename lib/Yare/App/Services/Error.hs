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
  | NoSingletonInputs
  | ScriptAddressNoPublicKeyHash
  | TxAutoBalanceError (InAnyShelleyBasedEra TxBodyErrorAutoBalance)

instance Show TxConstructionError where
  show = \case
    NoFeeInputs →
      "No fee inputs available."
    NoCollateralInputs →
      "No collateral inputs available."
    NoSingletonInputs →
      "No inputs to use as a singleton available."
    ScriptAddressNoPublicKeyHash →
      "Script address has no public key hash."
    TxAutoBalanceError (InAnyShelleyBasedEra era e) →
      "Tx balancing error in era " <> show era <> ": " <> show e
