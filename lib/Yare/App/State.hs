module Yare.App.State
  ( initialState
  , State
  ) where

import Yare.Prelude hiding (State)

import Cardano.Api.Shelley (ScriptHash, TxId)
import Yare.App.Services.DeployScript (ScriptStatus)
import Yare.Chain.Follower (ChainStateᵣ, initialChainState)
import Yare.Chain.Types (SyncFrom)

type Stateᵣ =
  SyncFrom
    : Tagged "in-ledger" [TxId]
    : Tagged "submitted" [TxId]
    : Map ScriptHash ScriptStatus
    : ChainStateᵣ

type State = HList Stateᵣ

initialState ∷ SyncFrom ∈ config ⇒ config → State
initialState config =
  syncFrom
    .*. inLedger
    .*. submitted
    .*. scriptStatuses
    .*. initialChainState
 where
  inLedger ∷ Tagged "in-ledger" [TxId] = Tagged []
  submitted ∷ Tagged "submitted" [TxId] = Tagged []
  syncFrom ∷ SyncFrom = look config
  scriptStatuses ∷ Map ScriptHash ScriptStatus = mempty
