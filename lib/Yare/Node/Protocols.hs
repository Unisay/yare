module Yare.Node.Protocols (makeNodeToClientProtocols) where

import Yare.Prelude hiding (atomically)

import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Client.Subscription (MuxMode (..))
import Cardano.Ledger.Crypto (StandardCrypto)
import Control.Tracer (nullTracer)
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
  )
import Ouroboros.Consensus.Node.ProtocolInfo (pClientInfoCodecConfig)
import Ouroboros.Network.Mux (RunMiniProtocol (..), mkMiniProtocolCbFromPeer)
import Ouroboros.Network.NodeToClient
  ( MinimalInitiatorContext
  , NodeToClientProtocols (..)
  , NodeToClientVersion
  , ResponderContext
  , localTxMonitorPeerNull
  )
import Ouroboros.Network.Protocol.ChainSync.Client (chainSyncClientPeer)
import Ouroboros.Network.Protocol.LocalStateQuery.Client
  ( localStateQueryClientPeer
  )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
  ( localTxSubmissionClientPeer
  )
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Follower (ChainFollower (..))
import Yare.Chain.Point (ChainPoint)
import Yare.Chain.Sync qualified as ChainSync
import Yare.Query qualified as Query
import Yare.Submitter qualified as Submitter
import Yare.Tracer (debugTracer, withPrefix)

makeNodeToClientProtocols
  ∷ ∀ ntcAddr
   . ChainFollower IO
  → Maybe ChainPoint
  → Query.Q
  → Submitter.Q
  → NodeToClientVersion
  → BlockNodeToClientVersion (CardanoBlock StandardCrypto)
  → NodeToClientProtocols InitiatorMode ntcAddr LByteString IO () Void
makeNodeToClientProtocols
  chainFollower
  syncFrom
  queryQ
  submitQ
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
        , chainSyncClientPeer $
            ChainSync.client chainFollower (maybeToList syncFrom)
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
        , localTxSubmissionClientPeer $ Submitter.client submitQ
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
      InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer \_context →
        ( show >$< withPrefix "Query" debugTracer
        , cStateQueryCodec
        , localStateQueryClientPeer $ Query.client queryQ
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
      ∷ ClientCodecs StdCardanoBlock IO = clientCodecs codecConfig blockVer n2cVer

    codecConfig ∷ CodecConfig StdCardanoBlock =
      let byronEpochSlots = EpochSlots 21600
       in pClientInfoCodecConfig (protocolClientInfoCardano byronEpochSlots)
