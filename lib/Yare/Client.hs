{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use const" #-}
module Yare.Client (main) where

import Relude hiding (atomically)

import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Client.Subscription (WithMuxBearer, subscribe)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Mnemonic (MkMnemonicError)
import Codec.CBOR.Term qualified as CBOR
import Control.Concurrent.Class.MonadSTM.TQueue (TQueue, writeTQueue)
import Control.Monad.Class.MonadAsync (Concurrently (..))
import Control.Monad.Class.MonadSTM (MonadSTM (atomically))
import Control.Monad.Morph (MFunctor (hoist))
import Control.Monad.Oops (Variant)
import Control.Monad.Oops qualified as Oops
import Control.Tracer (Tracer, debugTracer, nullTracer)
import Data.ByteString.Lazy qualified as BSL
import Data.Tagged (Tagged)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Ouroboros.Consensus.Block.Abstract (CodecConfig, Point)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, EraMismatch)
import Ouroboros.Consensus.Cardano.Node (protocolClientInfoCardano)
import Ouroboros.Consensus.Ledger.Query (Query)
import Ouroboros.Consensus.Network.NodeToClient
  ( Codecs' (..)
  , cTxSubmissionCodec
  , clientCodecs
  )
import Ouroboros.Consensus.Node.ErrorPolicy (consensusErrorPolicy)
import Ouroboros.Consensus.Node.NetworkProtocolVersion
  ( HasNetworkProtocolVersion (..)
  , supportedNodeToClientVersions
  )
import Ouroboros.Consensus.Node.ProtocolInfo (pClientInfoCodecConfig)
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block (Tip)
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mux (MuxMode (..), RunMiniProtocol (..), mkMiniProtocolCbFromPeer)
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
import Path (Abs, File, Path)
import String.ANSI (blackBg, faint)
import Text.ANSI (red)
import Text.Pretty.Simple (pPrint)
import Yare.Addresses (Error (..))
import Yare.Addresses qualified as Addresses
import Yare.Chain.Block (HFBlock, IxedByBlock (..))
import Yare.Chain.Follower (initialChainState)
import Yare.Chain.Point (ChainPoint)
import Yare.Http.Server qualified as Http
import Yare.LSQ (QueryCont (..), queryLedgerTip)
import Yare.LSQ qualified as LSQ
import Yare.Node.Clients (NodeClients (..), mkNodeClients)
import Yare.Node.Interface (NodeInterface (..), newNodeInterfaceIO)
import Yare.Node.Socket (NodeSocket, nodeSocketLocalAddress)
import Yare.Storage (ioRefStorage)

--------------------------------------------------------------------------------

main
  ∷ NodeSocket
  → NetworkMagic
  → Tagged "mnemonic" (Path Abs File)
  → Maybe ChainPoint
  → IO ()
main nodeSocket netMagic mnemonicFile syncFrom = withHandledErrors do
  addresses ← Addresses.deriveFromMnemonic netMagic mnemonicFile
  storage ← ioRefStorage <$> newIORef initialChainState
  nodeIface ← liftIO $ newNodeInterfaceIO addresses storage
  let nodeClients = mkNodeClients nodeIface syncFrom

  void . liftIO . runConcurrently . foldMap Concurrently $
    [ runLocalStateQueries (localStateQueryQ nodeIface)
    , Warp.run 9999 (simpleCors (Http.application storage))
    , absurd <$> stayConnectedToNode nodeSocket netMagic nodeClients
    ]

runLocalStateQueries ∷ TQueue IO (QueryCont IO) → IO ()
runLocalStateQueries q = do
  atomically . writeTQueue q $
    QueryCont (hoist handleQueryErrors queryLedgerTip) \case
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

stayConnectedToNode ∷ NodeSocket → NetworkMagic → NodeClients → IO Void
stayConnectedToNode nodeSocket netMagic NodeClients {..} =
  withIOManager \ioManager →
    subscribe
      (localSnocket ioManager)
      netMagic
      (supportedNodeToClientVersions (Proxy @HFBlock))
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
            networkErrorPolicies <> consensusErrorPolicy (Proxy @HFBlock)
        }
      protocols
 where
  protocols
    ∷ NodeToClientVersion
    → BlockNodeToClientVersion (CardanoBlock StandardCrypto)
    → NodeToClientProtocols 'InitiatorMode LocalAddress BSL.ByteString IO () Void
  protocols nodeToClientVer blockVer =
    let Codecs {cChainSyncCodec, cTxSubmissionCodec, cStateQueryCodec, cTxMonitorCodec} =
          clientCodecs codecConfig blockVer nodeToClientVer
     in NodeToClientProtocols
          { localChainSyncProtocol =
              InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer \_context →
                (lcsTracer, cChainSyncCodec, CS.chainSyncClientPeer chainSync)
          , localTxSubmissionProtocol =
              InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer \_context →
                (nullTracer, cTxSubmissionCodec, localTxSubmissionPeerNull)
          , localStateQueryProtocol =
              InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer \_context →
                ( lsqTracer
                , cStateQueryCodec
                , LSQ.localStateQueryClientPeer localState
                )
          , localTxMonitorProtocol =
              InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer \_context →
                (nullTracer, cTxMonitorCodec, localTxMonitorPeerNull)
          }

  codecConfig ∷ CodecConfig HFBlock =
    let byronEpochSlots = EpochSlots 21600
     in pClientInfoCodecConfig (protocolClientInfoCardano byronEpochSlots)

  lcsTracer
    ∷ Tracer IO (TraceSendRecv (ChainSync HFBlock (Point HFBlock) (Tip HFBlock))) =
      nullTracer -- showTracer "LCS"
  lsqTracer
    ∷ Tracer
        IO
        (TraceSendRecv (LocalStateQuery HFBlock (Point HFBlock) (Query HFBlock))) =
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

--------------------------------------------------------------------------------
-- Error handling --------------------------------------------------------------

withHandledErrors
  ∷ ExceptT (Variant [Addresses.Error, MkMnemonicError 8]) IO ()
  → IO ()
withHandledErrors =
  crashOnAddressesError
    >>> crashOnMnemonicError
    >>> Oops.runOops

crashOnAddressesError
  ∷ ExceptT (Variant (Addresses.Error : e)) IO ()
  → ExceptT (Variant e) IO ()
crashOnAddressesError = Oops.catch \(NetworkMagicNoTag magic) → do
  crash $ "Failed to determine a network tag for magic: " <> show magic

crashOnMnemonicError
  ∷ ExceptT (Variant (MkMnemonicError 8 : e)) IO ()
  → ExceptT (Variant e) IO ()
crashOnMnemonicError = Oops.catch \(err ∷ MkMnemonicError 8) →
  crash $ "Failed to parse mnemonic file: " <> show err

handleQueryErrors
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
handleQueryErrors =
  crashOnEraMismatch
    >>> crashOnUnknownEraIndex
    >>> crashOnNoLedgerTipQuery
    >>> Oops.runOops

crashOnEraMismatch
  ∷ ExceptT (Variant (EraMismatch : e)) IO a
  → ExceptT (Variant e) IO a
crashOnEraMismatch = Oops.catch \(err ∷ EraMismatch) →
  crash $ "Local state query mismatch: " <> show err

crashOnUnknownEraIndex
  ∷ ExceptT (Variant (LSQ.UnknownEraIndex : e)) IO a
  → ExceptT (Variant e) IO a
crashOnUnknownEraIndex = Oops.catch \(err ∷ LSQ.UnknownEraIndex) →
  crash $ "Local state query unknown era: " <> show err

crashOnNoLedgerTipQuery
  ∷ ExceptT (Variant (LSQ.NoLedgerTipQueryInByronEra : e)) IO a
  → ExceptT (Variant e) IO a
crashOnNoLedgerTipQuery = Oops.catch \(err ∷ LSQ.NoLedgerTipQueryInByronEra) →
  crash $ "No ledger tip query in Byron era: " <> show err

crash ∷ MonadIO m ⇒ Text → m a
crash err = liftIO $ exitFailure <* putTextLn (red err)
