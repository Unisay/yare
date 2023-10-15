module Yare.Node.Clients
  ( NodeClients (..)
  , mkNodeClients
  ) where

import Relude hiding (atomically)

import Ouroboros.Consensus.Ledger.Query (Query)
import Ouroboros.Consensus.Protocol.Praos.Translate ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Protocol.ChainSync.Client (ChainSyncClient)
import Ouroboros.Network.Protocol.LocalStateQuery.Client (LocalStateQueryClient)
import Yare.Chain.Block (HFBlock)
import Yare.Chain.Sync (chainSyncClient)
import Yare.Chain.Types (ChainPoint, ChainTip)
import Yare.LSQ (localStateQueryClient)
import Yare.Node.Interface (NodeInterface (..))

data NodeClients = NodeClients
  { chainSync ∷ ChainSyncClient HFBlock ChainPoint ChainTip IO ()
  , localState ∷ LocalStateQueryClient HFBlock ChainPoint (Query HFBlock) IO ()
  }

mkNodeClients ∷ NodeInterface IO → Maybe ChainPoint → NodeClients
mkNodeClients NodeInterface {chainFollower, localStateQueryQ} syncFrom =
  NodeClients
    { chainSync = chainSyncClient chainFollower (maybeToList syncFrom)
    , localState = localStateQueryClient localStateQueryQ
    }
