module Yare.Funds
  ( Funds
  , FundsRow
  , HasFunds
  , useFeeInputs
  , useCollateralInputs
  ) where

import Yare.Prelude

import Cardano.Api.Shelley (TxIn, Value)
import Data.List.NonEmpty qualified as NE
import Data.Row.Records qualified as Rec
import Yare.Address (Addresses)
import Yare.Address qualified as Addresses
import Yare.Address.Derivation (AddressWithKey (ledgerAddress))
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo

type FundsRow =
  {--} ("addresses" .== Addresses)
    .+ ("utxo" .== Utxo)

type Funds = Rec FundsRow
type HasFunds r = Open FundsRow r
  

useFeeInputs
  ∷ HasFunds r
  ⇒ Rec r
  → Maybe (Rec r, NonEmpty (TxIn, (AddressWithKey, Value)))
useFeeInputs funds = do
  feeInputs ← NE.nonEmpty (first (const feeAddress) <<$>> feeEntries)
  pure (funds', feeInputs)
 where
  funds' =
    funds & Rec.update #addresses addresses' & Rec.update #utxo utxo'
  (addresses', feeAddress) =
    Addresses.useForFees (funds .! #addresses)
  (utxo', feeEntries) =
    Utxo.useByAddress (funds .! #utxo) (ledgerAddress feeAddress)

useCollateralInputs
  ∷ HasFunds r
  ⇒ Rec r
  → Maybe (Rec r, NonEmpty (TxIn, (AddressWithKey, Value)))
useCollateralInputs funds = do
  colInputs ← NE.nonEmpty collateralEntries
  pure (funds', first (const collateralAddressWithKey) <<$>> colInputs)
 where
  funds' =
    funds
      & Rec.update #addresses addresses'
      & Rec.update #utxo utxo'
  (addresses', collateralAddressWithKey) =
    Addresses.useForCollateral (funds .! #addresses)
  (utxo', collateralEntries) =
    Utxo.useByAddress (funds .! #utxo) collateralAddr
  collateralAddr =
    ledgerAddress collateralAddressWithKey
