{-# OPTIONS_GHC -Wno-orphans #-}

module Yare.App.State
  ( initialState
  , State
  ) where

import Yare.Prelude hiding (State)

import Cardano.Api.Shelley (ScriptHash, TxId)
import Control.DeepSeq.Orphans ()
import Data.Map.Strict qualified as Map
import Yare.App.Services.DeployScript (ScriptStatus)
import Yare.Chain.Follower (ChainStateᵣ, initialChainState)
import Yare.Chain.Types (SyncFrom)

type Stateᵣ =
  Tagged "submitted" [TxId]
    : Tagged "in-ledger" [TxId]
    : Map ScriptHash ScriptStatus
    : SyncFrom
    : ChainStateᵣ

type State = HList Stateᵣ

initialState ∷ SyncFrom ∈ config ⇒ config → State
initialState config =
  submitted
    `strictHCons` inLedger
    `strictHCons` scriptStatuses
    `strictHCons` syncFrom
    `strictHCons` initialChainState
 where
  inLedger ∷ Tagged "in-ledger" [TxId] = Tagged []
  submitted ∷ Tagged "submitted" [TxId] = Tagged []
  syncFrom ∷ SyncFrom = look config
  scriptStatuses ∷ Map ScriptHash ScriptStatus = Map.empty
