module Yare.Node.Subscription (start) where

import Yare.Prelude

import Cardano.Api (NetworkMagic, TxId)
import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Client.Subscription
  ( SubscriptionTrace
      ( SubscriptionError
      , SubscriptionReconnect
      , SubscriptionResult
      , SubscriptionTerminate
      )
  )
import Cardano.Ledger.Crypto (StandardCrypto)
import Codec.Serialise.Class.Orphans ()
import Control.Exception (mask, try)
import Control.Monad.Class.MonadTimer.SI (MonadDelay (threadDelay))
import Control.Tracer (Tracer, traceWith)
import Control.Tracer.Extended
  ( debugTracer
  , nullTracer
  , withFaint
  , withPrefix
  )
import Data.Map.Strict qualified as Map
import Data.Maybe.Strict (StrictMaybe)
import Fmt.Orphans ()
import Network.Mux.Types (Mode (InitiatorMode))
import Ouroboros.Consensus.Block.Abstract (CodecConfig)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import Ouroboros.Consensus.Cardano.Node (protocolClientInfoCardano)
import Ouroboros.Consensus.Network.NodeToClient
  ( ClientCodecs
  , Codecs' (..)
  , cTxSubmissionCodec
  , clientCodecs
  )
import Ouroboros.Consensus.Node.NetworkProtocolVersion
  ( HasNetworkProtocolVersion (..)
  , supportedNodeToClientVersions
  )
import Ouroboros.Consensus.Node.ProtocolInfo (pClientInfoCodecConfig)
import Ouroboros.Network.Mux
  ( OuroborosApplicationWithMinimalCtx
  , RunMiniProtocol (..)
  , mkMiniProtocolCbFromPeer
  , mkMiniProtocolCbFromPeerSt
  )
import Ouroboros.Network.NodeToClient
  ( IOManager
  , LocalAddress (..)
  , MinimalInitiatorContext
  , NetworkConnectTracers (..)
  , NodeToClientProtocols (..)
  , NodeToClientVersion
  , NodeToClientVersionData (..)
  , ResponderContext
  , Versions
  , connectTo
  , foldMapVersions
  , localSnocket
  , localTxMonitorPeerNull
  , versionedNodeToClientProtocols
  , withIOManager
  )
import Ouroboros.Network.Protocol.ChainSync.Client (chainSyncClientPeer)
import Ouroboros.Network.Protocol.LocalStateQuery.Client
  ( localStateQueryClientPeer
  )
import Ouroboros.Network.Protocol.LocalStateQuery.Type (State (StateIdle))
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
  ( localTxSubmissionClientPeer
  )
import Yare.Address (Addresses)
import Yare.App.Types qualified as Yare
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Block.Reference (blockRefPoint)
import Yare.Chain.Follower (ChainFollower (..), newChainFollower)
import Yare.Chain.Point (ChainPoint)
import Yare.Chain.Sync qualified as ChainSync
import Yare.Chain.Types (ChainTip, LastIndexedBlock)
import Yare.Node.Socket (nodeSnocketFilePath)
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
  → IO ()
start env =
  withIOManager \ioManager →
    mask \unmask →
      let
        loop ∷ IO ()
        loop = do
          subscriptionResult ← join <$> try (unmask (connectToNode ioManager))
          case subscriptionResult of
            Left e → do
              traceWith subTracer (SubscriptionError e)
              traceWith subTracer SubscriptionReconnect
              threadDelay 5
              loop
            Right () → do
              traceWith subTracer (SubscriptionResult ())
              traceWith subTracer SubscriptionTerminate
       in
        loop
 where
  subTracer ∷ Tracer IO (SubscriptionTrace ())
  subTracer = show >$< withPrefix "SUB" (withFaint debugTracer)

  connectToNode ∷ IOManager → IO (Either SomeException ())
  connectToNode ioManager =
    connectTo
      (localSnocket ioManager)
      NetworkConnectTracers
        { nctMuxTracer = nullTracer
        , nctHandshakeTracer =
            show >$< withPrefix "HS_" (withFaint debugTracer)
        }
      ( foldMapVersions
          applyVersion
          (Map.toList (supportedNodeToClientVersions (Proxy @StdCardanoBlock)))
      )
      (nodeSnocketFilePath (look env))

  applyVersion
    ∷ ( NodeToClientVersion
      , BlockNodeToClientVersion (CardanoBlock StandardCrypto)
      )
    → Versions
        NodeToClientVersion
        NodeToClientVersionData
        ( OuroborosApplicationWithMinimalCtx
            InitiatorMode
            LocalAddress
            LByteString
            IO
            ()
            Void
        )
  applyVersion (version, blockVersion) =
    versionedNodeToClientProtocols
      version
      NodeToClientVersionData
        { networkMagic = look @NetworkMagic env
        , query = False
        }
      ( nodeToClientProtocols
          (newChainFollower @state env)
          (look @Query.Q env)
          (look @Submitter.Q env)
          knownChainPoints
          version
          blockVersion
      )

  knownChainPoints ∷ IO [ChainPoint]
  knownChainPoints = do
    yareState ← readDefaultStorage @state env
    let lastIndexedBlockRef = lookTagged @"last-indexed" yareState
        lastIndexed = blockRefPoint <$> lastIndexedBlockRef
        syncFrom = lookTagged @"syncFrom" env
    pure . toList @StrictMaybe $ lastIndexed <|> syncFrom

nodeToClientProtocols
  ∷ ∀ ntcAddr
   . ChainFollower IO
  -- ^ Handles new blocks and rollbacks
  → Query.Q
  -- ^ Handles local state queries
  → Submitter.Q
  -- ^ Handles tx submission
  → IO [ChainPoint]
  -- ^ known chain points
  → NodeToClientVersion
  → BlockNodeToClientVersion (CardanoBlock StandardCrypto)
  → NodeToClientProtocols InitiatorMode ntcAddr LByteString IO () Void
nodeToClientProtocols
  chainFollower
  qryQ
  submitQ
  knownChainPoints
  n2cVer
  blockVer =
    NodeToClientProtocols
      { localChainSyncProtocol
      , localTxSubmissionProtocol
      , localStateQueryProtocol
      , localTxMonitorProtocol
      }
   where
    localChainSyncProtocol
      ∷ RunMiniProtocol
          InitiatorMode
          (MinimalInitiatorContext ntcAddr)
          (ResponderContext ntcAddr)
          LByteString
          IO
          ()
          Void
    localChainSyncProtocol =
      InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer \_context →
        ( nullTracer
        , cChainSyncCodec
        , chainSyncClientPeer (ChainSync.client chainFollower knownChainPoints)
        )

    localTxSubmissionProtocol
      ∷ RunMiniProtocol
          InitiatorMode
          (MinimalInitiatorContext ntcAddr)
          (ResponderContext ntcAddr)
          LByteString
          IO
          ()
          Void
    localTxSubmissionProtocol =
      InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer \_context →
        ( nullTracer
        , cTxSubmissionCodec
        , localTxSubmissionClientPeer (Submitter.client submitQ)
        )

    localStateQueryProtocol
      ∷ RunMiniProtocol
          InitiatorMode
          (MinimalInitiatorContext ntcAddr)
          (ResponderContext ntcAddr)
          LByteString
          IO
          ()
          Void
    localStateQueryProtocol =
      InitiatorProtocolOnly $ mkMiniProtocolCbFromPeerSt \_context →
        ( show >$< withPrefix "Query" (withFaint debugTracer)
        , cStateQueryCodec
        , StateIdle
        , localStateQueryClientPeer (Query.client qryQ)
        )

    localTxMonitorProtocol =
      InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer \_context →
        ( nullTracer
        , cTxMonitorCodec
        , localTxMonitorPeerNull
        )

    Codecs
      { cChainSyncCodec
      , cTxSubmissionCodec
      , cStateQueryCodec
      , cTxMonitorCodec
      }
      ∷ ClientCodecs StdCardanoBlock IO =
        clientCodecs codecConfig blockVer n2cVer

    codecConfig ∷ CodecConfig StdCardanoBlock =
      let byronEpochSlots = EpochSlots 21600
       in pClientInfoCodecConfig (protocolClientInfoCardano byronEpochSlots)
