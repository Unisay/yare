module Yare.Funds
  ( Funds
  , useFeeInputs
  , useCollateralInputs
  ) where

import Relude

import Cardano.Api.Shelley (TxIn, Value)
import Data.List.NonEmpty qualified as NE
import Yare.Addresses (Addresses)
import Yare.Addresses qualified as Addresses
import Yare.Chain.Types (LedgerAddress)
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo

type Funds ∷ Type
type Funds = (Addresses, Utxo)

useFeeInputs
  ∷ Funds
  → Maybe (Funds, NonEmpty (TxIn, (LedgerAddress, Value)))
useFeeInputs (addresses, utxo) =
  let (addresses', feeAddress) = Addresses.useForFees addresses
      (utxo', feeEntries) = Utxo.useByAddress utxo feeAddress
   in ((addresses', utxo'),) <$> NE.nonEmpty feeEntries

useCollateralInputs
  ∷ Funds
  → Maybe (Funds, NonEmpty (TxIn, (LedgerAddress, Value)))
useCollateralInputs (addresses, utxo) =
  let (addresses', collateralAddress) = Addresses.useForCollateral addresses
      (utxo', collateralEntries) = Utxo.useByAddress utxo collateralAddress
   in ((addresses', utxo'),) <$> NE.nonEmpty collateralEntries
