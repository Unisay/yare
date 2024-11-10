{-# OPTIONS_GHC -Wno-orphans #-}

module Yare.App.State
  ( initialState
  , State
  ) where

import Yare.Prelude hiding (State)

import Cardano.Api.Shelley (TxId)
import Control.DeepSeq.Orphans ()
import Data.Set qualified as Set
import Yare.Chain.Follower (ChainStateᵣ, initialChainState)
import Yare.Chain.Types (SyncFrom)

type Stateᵣ =
  Tagged "submitted" (Set TxId)
    : Tagged "in-ledger" (Set TxId)
    : SyncFrom
    : ChainStateᵣ

type State = HList Stateᵣ

initialState ∷ SyncFrom ∈ config ⇒ config → State
initialState config =
  submitted
    `strictHCons` inLedger
    `strictHCons` syncFrom
    `strictHCons` initialChainState
 where
  inLedger ∷ Tagged "in-ledger" (Set TxId) = Tagged Set.empty
  submitted ∷ Tagged "submitted" (Set TxId) = Tagged Set.empty
  syncFrom ∷ SyncFrom = look config
