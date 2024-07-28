{-# LANGUAGE TemplateHaskell #-}

module Yare.Chain.Follower
  ( ChainFollower (..)
  , newChainFollower
  , initialChainState
  , ChainState (..)
  , utxo
  , chainTip
  ) where

import Relude

import Control.Lens.TH (makeLenses)
import Ouroboros.Network.Block (Tip (TipGenesis))
import Yare.Addresses (Addresses)
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Types (ChainPoint, ChainTip)
import Yare.Storage (Storage (overStorage))
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo
import NoThunks.Class (NoThunks)

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
  { _utxo ∷ !Utxo
  , _chainTip ∷ !ChainTip
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NoThunks)

initialChainState ∷ ChainState
initialChainState =
  ChainState
    { _utxo = Utxo.initial
    , _chainTip = TipGenesis
    }

indexBlock ∷ Addresses → StdCardanoBlock → ChainTip → ChainState → ChainState
indexBlock addresses block tip ChainState {_utxo} =
  ChainState
    { _utxo = Utxo.indexBlock addresses block _utxo
    , _chainTip = tip
    }

rollbackTo ∷ ChainPoint → ChainTip → ChainState → ChainState
rollbackTo point tip ChainState {_utxo} =
  ChainState
    { _utxo = Utxo.rollbackTo point _utxo
    , _chainTip = tip
    }

$(makeLenses ''ChainState)
