module Yare.Funds
  ( Funds
  , SomeFunds
  , useFeeInputs
  , useCollateralInputs
  ) where

import Cardano.Api.Shelley (TxIn, Value)
import Data.List.NonEmpty qualified as NE
import Data.Row (Disjoint, Rec, (.!), type (.+), type (.==))
import Data.Row.Records qualified as Rec
import Relude hiding (get)
import Yare.Address (Addresses)
import Yare.Address qualified as Addresses
import Yare.Address.Derivation (AddressWithKey (ledgerAddress))
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo

type Funds = ("addresses" .== Addresses .+ "utxo" .== Utxo)
type SomeFunds r = Rec (Funds .+ r)

useFeeInputs
  ∷ Disjoint Funds r
  ⇒ SomeFunds r
  → Maybe (SomeFunds r, NonEmpty (TxIn, (AddressWithKey, Value)))
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
  ∷ Disjoint Funds r
  ⇒ SomeFunds r
  → Maybe (SomeFunds r, NonEmpty (TxIn, (AddressWithKey, Value)))
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
