{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Yare.Chain.Follower
  ( ChainFollower (..)
  , newChainFollower
  , initialChainState
  , ChainState
  , SomeChainState
  ) where

import Relude hiding (get)

import Data.Row (Disjoint, Rec, (.!), (.+), (.==), type (.+), type (.==))
import Data.Row.Records qualified as Rec
import Fmt (pretty)
import Ouroboros.Network.Block (Tip (TipGenesis))
import Yare.Address (Addresses)
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Types (ChainPoint, ChainTip)
import Yare.Storage (Storage (overStorage))
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo

type ChainFollower ∷ (Type → Type) → Type
data ChainFollower m = ChainFollower
  { onNewBlock ∷ StdCardanoBlock → ChainTip → m ()
  , onRollback ∷ ChainPoint → ChainTip → m ()
  }

newChainFollower
  ∷ Disjoint ChainState r
  ⇒ Addresses
  → Storage IO (SomeChainState r)
  → ChainFollower IO
newChainFollower addresses storage =
  ChainFollower
    { onNewBlock = \(block ∷ StdCardanoBlock) (tip ∷ ChainTip) →
        overStorage storage \chainState →
          (indexBlock addresses block tip chainState, ())
    , onRollback = \(point ∷ ChainPoint) (tip ∷ ChainTip) →
        overStorage storage \chainState →
          (rollbackTo point tip chainState, ())
    }

type ChainState = ("utxo" .== Utxo .+ "chainTip" .== ChainTip)
type SomeChainState r = Rec (ChainState .+ r)

initialChainState ∷ Rec ChainState
initialChainState = (#utxo .== Utxo.initial) .+ (#chainTip .== TipGenesis)

indexBlock
  ∷ Disjoint ChainState r
  ⇒ Addresses
  → StdCardanoBlock
  → ChainTip
  → SomeChainState r
  → SomeChainState r
indexBlock addresses block tip chainState =
  case Utxo.indexBlock addresses block (chainState .! #utxo) of
    Nothing →
      Rec.update #chainTip tip chainState
    Just utxo →
      trace (pretty utxo) $
        chainState
          & Rec.update #utxo utxo
          & Rec.update #chainTip tip

rollbackTo
  ∷ Disjoint ChainState r
  ⇒ ChainPoint
  → ChainTip
  → SomeChainState r
  → SomeChainState r
rollbackTo point tip chainState =
  case Utxo.rollbackTo point (chainState .! #utxo) of
    Nothing →
      Rec.update #chainTip tip chainState
    Just utxo →
      trace (pretty utxo) $
        chainState
          & Rec.update #utxo utxo
          & Rec.update #chainTip tip
