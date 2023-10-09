{-# LANGUAGE PartialTypeSignatures #-}

module Yare.Client (main) where

import Relude hiding (atomically)

import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Client.Subscription (WithMuxBearer, subscribe)
import Codec.CBOR.Term qualified as CBOR
import Control.Concurrent.Class.MonadSTM.TQueue (TQueue, writeTQueue)
import Control.Monad.Class.MonadAsync (concurrently_)
import Control.Monad.Class.MonadSTM (MonadSTM (atomically))
import Control.Monad.Morph (MFunctor (hoist))
import Control.Monad.Oops (Variant)
import Control.Monad.Oops qualified as Oops
import Control.Tracer (Tracer, debugTracer, nullTracer)
import Ouroboros.Consensus.Block.Abstract (CodecConfig, Point)
import Ouroboros.Consensus.Cardano.Block (EraMismatch)
import Ouroboros.Consensus.Cardano.Node (protocolClientInfoCardano)
import Ouroboros.Consensus.Ledger.Query (Query)
import Ouroboros.Consensus.Network.NodeToClient
  ( Codecs' (..)
  , cTxSubmissionCodec
  , clientCodecs
  )
import Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import Ouroboros.Consensus.Node.NetworkProtocolVersion
  ( supportedNodeToClientVersions
  )
import Ouroboros.Consensus.Node.ProtocolInfo (pClientInfoCodecConfig)
import Ouroboros.Consensus.Protocol.Praos.Translate ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block (Tip)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux (MuxPeer (..), RunMiniProtocol (..))
import Ouroboros.Network.NodeToClient
  ( ClientSubscriptionParams (..)
  , ConnectionId
  , ErrorPolicyTrace
  , Handshake
  , LocalAddress
  , NetworkSubscriptionTracers (..)
  , NodeToClientProtocols (..)
  , NodeToClientVersion
  , SubscriptionTrace
  , TraceSendRecv
  , WithAddr
  , localSnocket
  , localTxMonitorPeerNull
  , localTxSubmissionPeerNull
  , networkErrorPolicies
  , withIOManager
  )
import Ouroboros.Network.Protocol.ChainSync.Client qualified as CS
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as LSQ
import Ouroboros.Network.Protocol.LocalStateQuery.Type (LocalStateQuery)
import String.ANSI (blackBg, faint)
import Text.ANSI (red)
import Text.Pretty.Simple (pPrint)
import Yare.ChainSync (chainSyncClient)
import Yare.Data.Node.Interface (NodeInterface (..), newNodeInterfaceIO)
import Yare.Data.Node.Socket (NodeSocket, nodeSocketLocalAddress)
import Yare.LSQ (QueryCont (..), localStateQueryClient, queryLedgerTip)
import Yare.LSQ qualified as LSQ
import Yare.Types (Block, IxedByBlock (..))

--------------------------------------------------------------------------------

main ∷ NodeSocket → IO ()
main nodeSocket = do
  nodeInterface ← newNodeInterfaceIO
  concurrently_
    do runLocalStateQueries (localStateQueryQ nodeInterface)
    do stayConnectedToNode nodeSocket nodeInterface

runLocalStateQueries ∷ TQueue IO (QueryCont IO) → IO ()
runLocalStateQueries q = do
  atomically . writeTQueue q $
    QueryCont (hoist handleErrors queryLedgerTip) \case
      Left failure →
        crash $ "Local state query failed to acquire state: " <> show failure
      Right ns → case ns of
        IxedByBlockByron p → pPrint p
        IxedByBlockShelley p → pPrint p
        IxedByBlockAllegra p → pPrint p
        IxedByBlockMary p → pPrint p
        IxedByBlockAlonzo p → pPrint p
        IxedByBlockBabbage p → pPrint p
        IxedByBlockConway p → pPrint p
 where
  crash ∷ MonadIO m ⇒ Text → m a
  crash err = liftIO $ exitFailure <* putTextLn (red err)

  handleErrors
    ∷ ExceptT
        ( Variant
            [ EraMismatch
            , LSQ.UnknownEraIndex
            , LSQ.NoLedgerTipQueryInByronEra
            ]
        )
        IO
        a
    → IO a
  handleErrors =
    ( Oops.catch \(err ∷ EraMismatch) →
        crash $ "Local state query mismatch: " <> show err
    )
      >>> ( Oops.catch \(err ∷ LSQ.UnknownEraIndex) →
              crash $ "Local state query unknown era: " <> show err
          )
      >>> ( Oops.catch \(err ∷ LSQ.NoLedgerTipQueryInByronEra) →
              crash $ "No ledger tip query in Byron era: " <> show err
          )
      >>> Oops.runOops

stayConnectedToNode ∷ NodeSocket → NodeInterface IO → IO Void
stayConnectedToNode nodeSocket NodeInterface {..} = withIOManager \ioManager →
  subscribe
    (localSnocket ioManager)
    mainnet
    (supportedNodeToClientVersions (Proxy @Block))
    NetworkSubscriptionTracers
      { nsMuxTracer = nullTracer
      , nsHandshakeTracer = handshakeTracer
      , nsErrorPolicyTracer = errorPolicyTracer
      , nsSubscriptionTracer = subscriptionTracer
      }
    ClientSubscriptionParams
      { cspAddress = nodeSocketLocalAddress nodeSocket
      , cspConnectionAttemptDelay = Nothing
      , cspErrorPolicies =
          networkErrorPolicies <> consensusErrorPolicy (Proxy @Block)
      }
    \nodeToClientVer blockVer _connectionId →
      let Codecs {..} = clientCodecs codecConfig blockVer nodeToClientVer
       in NodeToClientProtocols
            { localChainSyncProtocol =
                InitiatorProtocolOnly . MuxPeer lcsTracer cChainSyncCodec $
                  CS.chainSyncClientPeer chainSyncClient
            , localTxSubmissionProtocol =
                InitiatorProtocolOnly . MuxPeer nullTracer cTxSubmissionCodec $
                  localTxSubmissionPeerNull
            , localStateQueryProtocol =
                InitiatorProtocolOnly . MuxPeer lsqTracer cStateQueryCodec $
                  LSQ.localStateQueryClientPeer $
                    localStateQueryClient localStateQueryQ
            , localTxMonitorProtocol =
                InitiatorProtocolOnly . MuxPeer nullTracer cTxMonitorCodec $
                  localTxMonitorPeerNull
            }
 where
  mainnet = NetworkMagic 1

  codecConfig ∷ CodecConfig Block =
    let byronEpochSlots = EpochSlots 21600
     in pClientInfoCodecConfig (protocolClientInfoCardano byronEpochSlots)

  lcsTracer
    ∷ Tracer IO (TraceSendRecv (ChainSync Block (Point Block) (Tip Block))) =
      nullTracer -- showTracer "LCS"
  lsqTracer
    ∷ Tracer
        IO
        (TraceSendRecv (LocalStateQuery Block (Point Block) (Query Block))) =
      showTracer "LSQ"

  handshakeTracer
    ∷ Tracer
        IO
        ( WithMuxBearer
            (ConnectionId LocalAddress)
            (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term))
        )
  handshakeTracer = showTracer "HS_"

  errorPolicyTracer ∷ Tracer IO (WithAddr LocalAddress ErrorPolicyTrace) =
    showTracer "Err"

  subscriptionTracer ∷ Tracer IO (Identity (SubscriptionTrace LocalAddress)) =
    runIdentity >$< showTracer "SUB"

  showTracer ∷ Show a ⇒ String → Tracer IO a
  showTracer prefix =
    faint . ((blackBg (" " <> prefix <> " ") <> " ") <>) . show >$< debugTracer
