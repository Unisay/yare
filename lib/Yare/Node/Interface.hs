module Yare.Node.Interface
  ( NodeInterface (..)
  , newNodeInterfaceIO
  ) where

import Control.Concurrent.Class.MonadSTM.TMVar (TMVar, newEmptyTMVarIO)
import Control.Concurrent.Class.MonadSTM.TQueue (TQueue, newTQueueIO)
import Ouroboros.Network.Block (Point)
import Relude (IO, pure)
import Yare.Addresses (Addresses)
import Yare.Chain.Block (Block)
import Yare.Chain.Follower (ChainFollower, newChainFollower)
import Yare.LSQ (QueryCont)
import Yare.Storage (Storage)
import Yare.Utxo.State (UtxoState)

data NodeInterface m = NodeInterface
  { localStateQueryQ ∷ TQueue m (QueryCont m)
  , chainTip ∷ TMVar m (Point Block)
  , chainFollower ∷ ChainFollower m
  }

newNodeInterfaceIO ∷ Addresses → Storage IO UtxoState → IO (NodeInterface IO)
newNodeInterfaceIO addresses storage = do
  localStateQueryQ ← newTQueueIO
  chainTip ← newEmptyTMVarIO
  pure NodeInterface {chainFollower = newChainFollower addresses storage, ..}
