module Yare.Chain.Follower
  ( ChainFollower (..)
  , newChainFollower
  , initialChainState
  , ChainState
  , utxoState
  , chainTip
  ) where

import Relude

import Ouroboros.Network.Block (Tip (TipGenesis))
import Yare.Addresses (Addresses)
import Yare.Chain.Block (HFBlock)
import Yare.Chain.Types (ChainPoint, ChainTip)
import Yare.Storage (Storage (modifyStorage))
import Yare.Utxo.State (UtxoState)
import Yare.Utxo.State qualified as UtxoState

data ChainFollower m = ChainFollower
  { onNewBlock ∷ HFBlock → ChainTip → m ()
  , onRollback ∷ ChainPoint → ChainTip → m ()
  }

newChainFollower ∷ Addresses → Storage IO ChainState → ChainFollower IO
newChainFollower addresses storage =
  ChainFollower
    { onNewBlock = \(block ∷ HFBlock) (tip ∷ ChainTip) →
        modifyStorage storage (indexBlock addresses block tip)
    , onRollback = \(point ∷ ChainPoint) (tip ∷ ChainTip) →
        modifyStorage storage (rollbackTo point tip)
    }

data ChainState = ChainState
  { utxoState ∷ UtxoState
  , chainTip ∷ ChainTip
  }
  deriving stock (Eq, Show)

initialChainState ∷ ChainState
initialChainState =
  ChainState
    { utxoState = UtxoState.initialState
    , chainTip = TipGenesis
    }

indexBlock ∷ Addresses → HFBlock → ChainTip → ChainState → ChainState
indexBlock addresses block tip s =
  ChainState
    { utxoState = UtxoState.indexBlock addresses block (utxoState s)
    , chainTip = tip
    }

rollbackTo ∷ ChainPoint → ChainTip → ChainState → ChainState
rollbackTo point tip s =
  ChainState
    { utxoState = UtxoState.rollbackTo point (utxoState s)
    , chainTip = tip
    }
