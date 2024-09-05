module Yare.Funds
  ( Funds
  , useFeeInputs
  , useCollateralInputs
  ) where

import Relude

import Cardano.Api.Shelley (TxIn)
import Data.List.NonEmpty qualified as NE
import Yare.Addresses (Addresses)
import Yare.Addresses qualified as Addresses
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo

type Funds ∷ Type
type Funds = (Addresses, Utxo)

useFeeInputs ∷ Funds → Maybe (Funds, NonEmpty TxIn)
useFeeInputs (addresses, utxo) =
  let (addresses', feeAddress) = Addresses.useForFees addresses
      (utxo', feeInputs) = Utxo.useByAddress utxo feeAddress
   in ((addresses', utxo'),) <$> NE.nonEmpty feeInputs

useCollateralInputs ∷ Funds → Maybe (Funds, NonEmpty TxIn)
useCollateralInputs (addresses, utxo) =
  let (addresses', collateralAddress) = Addresses.useForCollateral addresses
      (utxo', collateralInputs) = Utxo.useByAddress utxo collateralAddress
   in ((addresses', utxo'),) <$> NE.nonEmpty collateralInputs
