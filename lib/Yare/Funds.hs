module Yare.Funds
  ( Funds
  , useFeeInputs
  , useCollateralInputs
  ) where

import Relude

import Cardano.Api.Shelley (TxIn, Value)
import Data.List.NonEmpty qualified as NE
import Yare.Address (AddressWithKey (ledgerAddress), Addresses)
import Yare.Address qualified as Addresses
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo

type Funds ∷ Type
type Funds = (Addresses, Utxo)

useFeeInputs ∷ Funds → Maybe (Funds, NonEmpty (TxIn, (AddressWithKey, Value)))
useFeeInputs (addresses, utxo) =
  NE.nonEmpty (first (const feeAddress) <<$>> feeEntries)
    <&> ((addresses', utxo'),)
 where
  (addresses', feeAddress) = Addresses.useForFees addresses
  (utxo', feeEntries) = Utxo.useByAddress utxo (ledgerAddress feeAddress)

useCollateralInputs
  ∷ Funds
  → Maybe (Funds, NonEmpty (TxIn, (AddressWithKey, Value)))
useCollateralInputs (addresses, utxo) =
  NE.nonEmpty (first (const collateralAddressWithKey) <<$>> collateralEntries)
    <&> ((addresses', utxo'),)
 where
  (addresses', collateralAddressWithKey) = Addresses.useForCollateral addresses
  (utxo', collateralEntries) = Utxo.useByAddress utxo collateralAddr
  collateralAddr = ledgerAddress collateralAddressWithKey
