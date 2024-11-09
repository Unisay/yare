{-# OPTIONS_GHC -Wno-orphans #-}

module Yare.App.State
  ( initialState
  , State
  ) where

import Yare.Prelude hiding (State)

import Cardano.Api.Shelley (ScriptHash, TxId)
import Control.DeepSeq.Orphans ()
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Yare.Chain.Follower (ChainStateᵣ, initialChainState)
import Yare.Chain.Types (SyncFrom)
import Yare.Utxo (ScriptDeployment)

type Stateᵣ =
  Tagged "submitted" (Set TxId)
    : Tagged "in-ledger" (Set TxId)
    : Map ScriptHash ScriptDeployment
    : SyncFrom
    : ChainStateᵣ

type State = HList Stateᵣ

initialState ∷ SyncFrom ∈ config ⇒ config → State
initialState config =
  submitted
    `strictHCons` inLedger
    `strictHCons` scriptDeployments
    `strictHCons` syncFrom
    `strictHCons` initialChainState
 where
  inLedger ∷ Tagged "in-ledger" (Set TxId) = Tagged Set.empty
  submitted ∷ Tagged "submitted" (Set TxId) = Tagged Set.empty
  syncFrom ∷ SyncFrom = look config
  scriptDeployments ∷ Map ScriptHash ScriptDeployment = Map.empty
