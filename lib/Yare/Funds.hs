module Yare.Funds
  ( Funds
  , useFeeInputs
  , useCollateralInputs
  ) where

import Relude

import Cardano.Api.Shelley (TxIn)
import Yare.Addresses (Addresses)
import Yare.Utxo.State (UtxoState)

type Funds ∷ Type
type Funds = (Addresses, UtxoState)

useFeeInputs ∷ Funds → (Funds, NonEmpty TxIn)
useFeeInputs = undefined $ error "useFeeInputs: not implemented"

useCollateralInputs ∷ Funds → (Funds, NonEmpty TxIn)
useCollateralInputs = undefined $ error "useCollateralInputs: not implemented"
