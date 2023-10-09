module Yare.Data.Node.Interface
  ( NodeInterface (..)
  , newNodeInterfaceIO
  ) where

import Control.Concurrent.Class.MonadSTM.TMVar (TMVar, newEmptyTMVarIO)
import Control.Concurrent.Class.MonadSTM.TQueue (TQueue, newTQueueIO)
import Ouroboros.Network.Block (Point)
import Relude (IO, pure)
import Yare.LSQ (QueryCont)
import Yare.Types (Block)

data NodeInterface m = NodeInterface
  { localStateQueryQ ∷ TQueue m (QueryCont m)
  , chainTip ∷ TMVar m (Point Block)
  }

newNodeInterfaceIO ∷ IO (NodeInterface IO)
newNodeInterfaceIO = do
  localStateQueryQ ← newTQueueIO
  chainTip ← newEmptyTMVarIO
  pure NodeInterface {..}
