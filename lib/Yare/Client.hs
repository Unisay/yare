{-# LANGUAGE PartialTypeSignatures #-}

module Yare.Client (main) where

import Relude hiding (atomically)

import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Client.Subscription (subscribe)
import Control.Concurrent.Class.MonadSTM.TQueue
  ( TQueue
  , newTQueueIO
  , writeTQueue
  )
import Control.Monad.Class.MonadAsync (concurrently_)
import Control.Monad.Class.MonadSTM (MonadSTM (atomically))
import Control.Monad.Morph (MFunctor (hoist))
import Control.Monad.Oops (Variant)
import Control.Monad.Oops qualified as Oops
import Control.Tracer (Tracer, debugTracer, nullTracer)
import Ouroboros.Consensus.Block.Abstract (CodecConfig)
import Ouroboros.Consensus.Cardano.Block (EraMismatch)
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
  , NetworkSubscriptionTracers (..)
  , NodeToClientProtocols (..)
  , chainSyncPeerNull
  , localSnocket
  , localTxMonitorPeerNull
  , localTxSubmissionPeerNull
  , networkErrorPolicies
  , withIOManager
  )
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as LSQ
import String.ANSI (faint)
import Text.ANSI (red)
import Text.Pretty.Simple (pPrint)
import Yare.Data.NodeSocket (NodeSocket, nodeSocketLocalAddress)
import Yare.LSQ (QueryCont (..), localStateQueryHandler, queryLedgerTip)
import Yare.LSQ qualified as LSQ
import Yare.Types (Block)

--------------------------------------------------------------------------------

main ∷ NodeSocket → IO ()
main nodeSocket = do
  localStateQueryQ ← newTQueueIO
  concurrently_
    do runLocalStateQueries localStateQueryQ
    do stayConnectedToNode nodeSocket localStateQueryQ

runLocalStateQueries ∷ TQueue IO (QueryCont IO) → IO ()
runLocalStateQueries q = do
  atomically . writeTQueue q $
    QueryCont (hoist handleErrors queryLedgerTip) \case
      Left failure →
        crash $ "Local state query failed to acquire state: " <> show failure
      Right ns →
        pPrint ns
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

stayConnectedToNode ∷ NodeSocket → TQueue IO (QueryCont IO) → IO Void
stayConnectedToNode nodeSocket localStateQueryQ = withIOManager \ioManager →
  subscribe
    (localSnocket ioManager)
    mainnet
    (supportedNodeToClientVersions (Proxy @Block))
    NetworkSubscriptionTracers
      { nsMuxTracer = showTracer
      , nsHandshakeTracer = showTracer
      , nsErrorPolicyTracer = showTracer
      , nsSubscriptionTracer = runIdentity >$< showTracer
      }
    ClientSubscriptionParams
      { cspAddress = nodeSocketLocalAddress nodeSocket
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
                  MuxPeer
                    nullTracer
                    cTxSubmissionCodec
                    localTxSubmissionPeerNull
            , localStateQueryProtocol =
                InitiatorProtocolOnly $
                  MuxPeer showTracer cStateQueryCodec $
                    LSQ.localStateQueryClientPeer $
                      localStateQueryHandler localStateQueryQ
            , localTxMonitorProtocol =
                InitiatorProtocolOnly $
                  MuxPeer nullTracer cTxMonitorCodec localTxMonitorPeerNull
            }
 where
  mainnet = NetworkMagic 1

  codecConfig ∷ CodecConfig Block =
    let byronEpochSlots = EpochSlots 21600
     in pClientInfoCodecConfig (protocolClientInfoCardano byronEpochSlots)

  showTracer ∷ Show a ⇒ Tracer IO a
  showTracer = faint . show >$< debugTracer
