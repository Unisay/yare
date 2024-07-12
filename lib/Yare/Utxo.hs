module Yare.Utxo
  ( Utxo
  , utxoEntries
  , fromList
  , spendableTxInputs
  , spendableValues
  , totalValue
  ) where

import Relude hiding (fromList)

import Cardano.Api (TxIn, Value)
import Data.Map.Strict qualified as Map
import Yare.Chain.Types (LedgerAddress)

type Utxo ∷ Type
newtype Utxo = Utxo (Map TxIn (LedgerAddress, Value))
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

utxoEntries ∷ Utxo → Map TxIn (LedgerAddress, Value)
utxoEntries (Utxo entries) = entries

fromList ∷ [(TxIn, (LedgerAddress, Value))] → Utxo
fromList = Utxo . Map.fromList

spendableTxInputs ∷ Utxo → Set TxIn
spendableTxInputs = Map.keysSet . spendableValues

spendableValues ∷ Utxo → Map TxIn Value
spendableValues (Utxo entries) = snd <$> entries

totalValue ∷ Utxo → Value
totalValue = fold . spendableValues
