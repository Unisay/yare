{-# LANGUAGE TemplateHaskell #-}

module Yare.Chain.Follower
  ( ChainFollower (..)
  , newChainFollower
  , initialChainState
  , ChainState (..)
  , utxoState
  , chainTip
  ) where

import Relude

import Control.Lens.TH (makeLenses)
import Ouroboros.Network.Block (Tip (TipGenesis))
import Yare.Addresses (Addresses)
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Types (ChainPoint, ChainTip)
import Yare.Storage (Storage (overStorage))
import Yare.Utxo.State (UtxoState)
import Yare.Utxo.State qualified as UtxoState

type ChainFollower ∷ (Type → Type) → Type
data ChainFollower m = ChainFollower
  { onNewBlock ∷ StdCardanoBlock → ChainTip → m ()
  , onRollback ∷ ChainPoint → ChainTip → m ()
  }

newChainFollower ∷ Addresses → Storage IO ChainState → ChainFollower IO
newChainFollower addresses storage =
  ChainFollower
    { onNewBlock = \(block ∷ StdCardanoBlock) (tip ∷ ChainTip) →
        overStorage storage ((,()) . indexBlock addresses block tip)
    , onRollback = \(point ∷ ChainPoint) (tip ∷ ChainTip) →
        overStorage storage ((,()) . rollbackTo point tip)
    }

type ChainState ∷ Type
data ChainState = ChainState
  { _utxoState ∷ UtxoState
  , _chainTip ∷ ChainTip
  }
  deriving stock (Eq, Show)

initialChainState ∷ ChainState
initialChainState =
  ChainState
    { _utxoState = UtxoState.initialState
    , _chainTip = TipGenesis
    }

indexBlock ∷ Addresses → StdCardanoBlock → ChainTip → ChainState → ChainState
indexBlock addresses block tip ChainState {_utxoState} =
  ChainState
    { _utxoState = UtxoState.indexBlock addresses block _utxoState
    , _chainTip = tip
    }

rollbackTo ∷ ChainPoint → ChainTip → ChainState → ChainState
rollbackTo point tip ChainState {_utxoState} =
  ChainState
    { _utxoState = UtxoState.rollbackTo point _utxoState
    , _chainTip = tip
    }

$(makeLenses ''ChainState)
