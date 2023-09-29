{-# LANGUAGE PartialTypeSignatures #-}

module Yare.Client (main) where

import Relude

import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Client.Subscription
  ( ConnectionId
  , LocalAddress
  , MuxTrace
  , WithMuxBearer
  , subscribe
  )
import Cardano.Ledger.Crypto (StandardCrypto)
import Codec.CBOR.Term qualified as CBOR
import Control.Tracer (Tracer, debugTracer, nullTracer)
import Ouroboros.Consensus.Block.Abstract (CodecConfig)
import Ouroboros.Consensus.Cardano (CardanoBlock)
import Ouroboros.Consensus.Cardano.Node (protocolClientInfoCardano)
import Ouroboros.Consensus.Network.NodeToClient
  ( Codecs' (..)
  , cTxSubmissionCodec
  , defaultCodecs
  )
import Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import Ouroboros.Consensus.Node.NetworkProtocolVersion
  ( supportedNodeToClientVersions
  )
import Ouroboros.Consensus.Node.ProtocolInfo (pClientInfoCodecConfig)
import Ouroboros.Consensus.Protocol.Praos.Translate ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux (MuxPeer (..), RunMiniProtocol (..))
import Ouroboros.Network.NodeToClient
  ( ClientSubscriptionParams (..)
  , ErrorPolicyTrace
  , Handshake
  , NetworkSubscriptionTracers (..)
  , NodeToClientProtocols (..)
  , NodeToClientVersion
  , SubscriptionTrace
  , TraceSendRecv
  , WithAddr
  , chainSyncPeerNull
  , localSnocket
  , localStateQueryPeerNull
  , localTxMonitorPeerNull
  , localTxSubmissionPeerNull
  , networkErrorPolicies
  , withIOManager
  )
import Ouroboros.Network.Snocket qualified as Snocket
import Path (Abs, File, Path, toFilePath)

type Block = CardanoBlock StandardCrypto

main ∷ Path Abs File → IO Void
main nodeSocketPath = withIOManager \ioManager →
  subscribe
    (localSnocket ioManager)
    mainnet
    (supportedNodeToClientVersions (Proxy @Block))
    NetworkSubscriptionTracers
      { nsMuxTracer = muxTracer
      , nsHandshakeTracer = handshakeTracer
      , nsErrorPolicyTracer = errorPolicyTracer
      , nsSubscriptionTracer = subscriptionTracer
      }
    ClientSubscriptionParams
      { cspAddress = Snocket.localAddressFromPath (toFilePath nodeSocketPath)
      , cspConnectionAttemptDelay = Nothing
      , cspErrorPolicies =
          networkErrorPolicies <> consensusErrorPolicy (Proxy @Block)
      }
    \nodeToClientVer blockVer _connectionId →
      let Codecs {..} = defaultCodecs codecConfig blockVer nodeToClientVer
       in NodeToClientProtocols
            { localChainSyncProtocol =
                InitiatorProtocolOnly $
                  MuxPeer nullTracer cChainSyncCodec chainSyncPeerNull
            , localTxSubmissionProtocol =
                InitiatorProtocolOnly $
                  MuxPeer nullTracer cTxSubmissionCodec localTxSubmissionPeerNull
            , localStateQueryProtocol =
                InitiatorProtocolOnly $
                  MuxPeer nullTracer cStateQueryCodec localStateQueryPeerNull
            , localTxMonitorProtocol =
                InitiatorProtocolOnly $
                  MuxPeer nullTracer cTxMonitorCodec localTxMonitorPeerNull
            }
 where
  mainnet = NetworkMagic 1

  codecConfig ∷ CodecConfig Block =
    let byronEpochSlots = EpochSlots 21600
     in pClientInfoCodecConfig (protocolClientInfoCardano byronEpochSlots)

  errorPolicyTracer ∷ Tracer IO (WithAddr LocalAddress ErrorPolicyTrace)
  errorPolicyTracer = show >$< debugTracer

  muxTracer ∷ (Show peer) ⇒ Tracer IO (WithMuxBearer peer MuxTrace)
  muxTracer = show >$< debugTracer

  subscriptionTracer ∷ Tracer IO (Identity (SubscriptionTrace LocalAddress))
  subscriptionTracer = show . runIdentity >$< debugTracer

  handshakeTracer
    ∷ Tracer
        IO
        ( WithMuxBearer
            (ConnectionId LocalAddress)
            (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term))
        )
  handshakeTracer = show >$< debugTracer
