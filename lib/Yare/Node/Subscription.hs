module Yare.Node.Subscription (start) where

import Yare.Prelude hiding (atomically)

import Cardano.Api.Shelley
  ( NetworkMagic
  , TxId
  )
import Cardano.Client.Subscription (subscribe)
import Codec.Serialise.Class.Orphans ()
import Control.Tracer.Extended
  ( debugTracer
  , nullTracer
  , withFaint
  , withPrefix
  )
import Fmt.Orphans ()
import Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import Ouroboros.Consensus.Node.NetworkProtocolVersion
  ( supportedNodeToClientVersions
  )
import Ouroboros.Network.NodeToClient
  ( ClientSubscriptionParams (..)
  , NetworkSubscriptionTracers (..)
  , localSnocket
  , networkErrorPolicies
  , withIOManager
  )
import Yare.Address (Addresses)
import Yare.App.Types qualified as Yare
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Follower (newChainFollower)
import Yare.Chain.Types (ChainTip, SyncFrom)
import Yare.Tracers ( Tracersᵣ )
import Yare.Node.Protocols (makeNodeToClientProtocols)
import Yare.Node.Socket (NodeSocket, nodeSocketLocalAddress)
import Yare.Query qualified as Query
import Yare.Storage (Storage (..))
import Yare.Submitter qualified as Submitter
import Yare.Utxo (Utxo)

-- | Connects to a Cardano Node socket and runs Node-to-Client mini-protocols.
start
  ∷ ∀ state env envᵣ
   . ( env ~ HList envᵣ
     , Yare.Configᵣ ∈∈ env
     , Tracersᵣ ∈∈ env
     , [Query.Q, Submitter.Q, Addresses, Storage IO state] ∈∈ env
     , [ Utxo
       , ChainTip
       , Tagged "submitted" [TxId]
       , Tagged "in-ledger" [TxId]
       , SyncFrom
       ]
        ∈∈ state
     )
  ⇒ env
  → IO Void
start env = withIOManager \ioManager → do
  yareState ← readStorage (look @(Storage IO state) env)
  subscribe
    (localSnocket ioManager)
    (look @NetworkMagic env)
    (supportedNodeToClientVersions (Proxy @StdCardanoBlock))
    NetworkSubscriptionTracers
      { nsMuxTracer =
          nullTracer
      , nsHandshakeTracer =
          show >$< withPrefix "HS_" (withFaint debugTracer)
      , nsErrorPolicyTracer =
          show >$< withPrefix "Err" (withFaint debugTracer)
      , nsSubscriptionTracer =
          show . runIdentity >$< withPrefix "SUB" (withFaint debugTracer)
      }
    ClientSubscriptionParams
      { cspAddress = nodeSocketLocalAddress (look @NodeSocket env)
      , cspConnectionAttemptDelay = Nothing
      , cspErrorPolicies =
          networkErrorPolicies
            <> consensusErrorPolicy (Proxy @StdCardanoBlock)
      }
    ( makeNodeToClientProtocols
        (newChainFollower @state env)
        (look @Query.Q env)
        (look @Submitter.Q env)
        ((<|>) <$> look @SyncFrom yareState <*> look @SyncFrom env)
    )
