{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Yare.App.State
  ( State
  , StateRow
  , HasAppState
  , initialState
  ) where

import Yare.Prelude hiding (State)

import Cardano.Api (TxId)
import NoThunks.Class (NoThunks (..), allNoThunks)
import Yare.Address (Addresses)
import Yare.Chain.Follower (initialChainState)
import Yare.Chain.Types (ChainTip)
import Yare.Utxo (Utxo)

type StateRow =
  {--} ("utxo" .== Utxo)
    .+ ("chainTip" .== ChainTip)
    .+ ("addresses" .== Addresses)
    .+ ("submitted" .== [TxId])

type State = Rec StateRow
type HasAppState r = Open StateRow r

instance (Forall r NoThunks, r ~ StateRow) ⇒ NoThunks (Rec r) where
  wNoThunks ctx = allNoThunks . erase @NoThunks (wNoThunks ctx)
  showTypeOf _proxy = "App.State"

initialState ∷ Addresses → State
initialState addresses' =
  initialChainState
    .+ (#addresses .== addresses')
    .+ (#submitted .== mempty)
