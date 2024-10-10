module Yare.Utxo
  ( Entries
  , Finality (..)
  , ScriptDeployment (..)
  , Update (..)
  , UpdateError (..)
  , Utxo

    -- * Updates
  , updateUtxo
  , rollback
  , finalise
  , setScriptDeployment

    -- * Queries
  , initial
  , allEntries
  , spendableEntries
  , spendableTxInputs
  , totalValue
  , useByAddress
  ) where

import Yare.Utxo.Internal
