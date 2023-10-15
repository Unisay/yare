module Yare.Chain.Follower
  ( ChainFollower (..)
  , newChainFollower
  ) where

import Relude

import Yare.Addresses (Addresses)
import Yare.Chain.Block (HFBlock)
import Yare.Chain.Types (ChainPoint, ChainTip)
import Yare.Storage (Storage (modifyStorage))
import Yare.Utxo.State (UtxoState, indexBlock, rollbackTo)

data ChainFollower m = ChainFollower
  { onNewBlock ∷ HFBlock → ChainTip → m ()
  , onRollback ∷ ChainPoint → m ()
  }

newChainFollower ∷ Addresses → Storage IO UtxoState → ChainFollower IO
newChainFollower addresses storage =
  ChainFollower
    { onNewBlock = \(block ∷ HFBlock) (tip ∷ ChainTip) →
        modifyStorage storage (indexBlock addresses block tip)
    , onRollback = \(chainPoint ∷ ChainPoint) →
        modifyStorage storage $ rollbackTo chainPoint
    }
