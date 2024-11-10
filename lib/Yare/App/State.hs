{-# OPTIONS_GHC -Wno-orphans #-}

module Yare.App.State
  ( initialState
  , State
  ) where

import Yare.Prelude hiding (State)

import Cardano.Api.Shelley (TxId)
import Control.DeepSeq.Orphans ()
import Data.Maybe.Strict (StrictMaybe (SNothing))
import Data.Set qualified as Set
import Yare.Chain.Follower (ChainStateᵣ, initialChainState)
import Yare.Chain.Types (LastIndexedBlock)

type Stateᵣ =
  Tagged "submitted" (Set TxId)
    : Tagged "in-ledger" (Set TxId)
    : LastIndexedBlock
    : ChainStateᵣ

type State = HList Stateᵣ

initialState ∷ State
initialState =
  submitted
    `strictHCons` inLedger
    `strictHCons` lastIndexed
    `strictHCons` initialChainState
 where
  lastIndexed ∷ LastIndexedBlock = Tagged SNothing
  inLedger ∷ Tagged "in-ledger" (Set TxId) = Tagged Set.empty
  submitted ∷ Tagged "submitted" (Set TxId) = Tagged Set.empty
