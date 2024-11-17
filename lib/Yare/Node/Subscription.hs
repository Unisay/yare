module Yare.Node.Subscription (start) where

import Yare.Prelude hiding (atomically)

import Cardano.Api.Shelley (NetworkMagic, TxId)
import Cardano.Client.Subscription (subscribe)
import Codec.Serialise.Class.Orphans ()
import Control.Tracer.Extended
  ( Tracer
  , debugTracer
  , nullTracer
  , withFaint
  , withPrefix
  )
import Fmt.Orphans ()
import Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import Ouroboros.Consensus.Node.NetworkProtocolVersion
  ( supportedNodeToClientVersions
  )
import Ouroboros.Network.ErrorPolicy (ErrorPolicies (..), ErrorPolicy (ErrorPolicy), SuspendDecision (Throw))
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
import Yare.Chain.Block.Reference (blockRefPoint)
import Yare.Chain.Follower (newChainFollower)
import Yare.Chain.Types (ChainTip, LastIndexedBlock)
import Yare.Node.Protocols (makeNodeToClientProtocols)
import Yare.Node.Socket (NodeSocket, nodeSocketLocalAddress)
import Yare.Query qualified as Query
import Yare.Storage (StorageMgr, readDefaultStorage)
import Yare.Submitter qualified as Submitter
import Yare.Tracers (Tracersᵣ)
import Yare.Utxo (Utxo)

-- | Connects to a Cardano Node socket and runs Node-to-Client mini-protocols.
start
  ∷ ∀ state env envᵣ
   . ( env ~ HList envᵣ
     , Yare.Configᵣ ∈∈ env
     , Tracersᵣ ∈∈ env
     , [Query.Q, Submitter.Q, Addresses, StorageMgr IO state] ∈∈ env
     , [ Utxo
       , ChainTip
       , LastIndexedBlock
       , Tagged "submitted" (Set TxId)
       , Tagged "in-ledger" (Set TxId)
       ]
        ∈∈ state
     )
  ⇒ env
  → IO Void
start env = withIOManager \ioManager → do
  yareState ← readDefaultStorage @state env
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
            <> yareErrorPolicies
      }
    ( makeNodeToClientProtocols
        (look @(Tracer IO SomeException) env)
        (newChainFollower @state env)
        (look @Query.Q env)
        (look @Submitter.Q env)
        ( let lastIndexed = lookTagged @"last-indexed" yareState
              syncFrom = lookTagged @"syncFrom" env
           in Tagged (fmap blockRefPoint lastIndexed <|> syncFrom)
        )
    )

yareErrorPolicies ∷ ErrorPolicies
yareErrorPolicies =
  ErrorPolicies
    { epAppErrorPolicies = [ErrorPolicy \(_ ∷ SomeException) → Just Throw]
    , epConErrorPolicies = []
    }
