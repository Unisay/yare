module Yare.App.State
  ( initialState
  , State
  ) where

import Yare.Prelude hiding (State)

import Cardano.Api.Shelley (TxId)
import Data.Strict.List (List (Nil))
import Yare.Chain.Follower (ChainStateᵣ, initialChainState)
import Yare.Chain.Types (SyncFrom)

type Stateᵣ = List TxId : SyncFrom : ChainStateᵣ
type State = HList Stateᵣ

initialState ∷ SyncFrom ∈ config ⇒ config → State
initialState config =
  submitted .*. syncFrom .*. initialChainState
 where
  submitted ∷ List TxId = Nil
  syncFrom ∷ SyncFrom = look config
