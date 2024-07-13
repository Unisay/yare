-- | Description: Yare application entry point
module Yare.App (start) where

import Relude hiding (atomically)

import Cardano.Api (NetworkMagic)
import Cardano.Chain.Slotting (EpochSlots (..))
import Cardano.Client.Subscription (MuxMode (..), WithMuxBearer, subscribe)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Mnemonic (MkMnemonicError)
import Codec.CBOR.Term qualified as CBOR
import Control.Concurrent.Class.MonadSTM.TQueue
  ( TQueue
  , newTQueueIO
  , writeTQueue
  )
import Control.Exception (throwIO)
import Control.Monad.Class.MonadAsync (Concurrently (..))
import Control.Monad.Class.MonadSTM (MonadSTM (atomically))
import Control.Monad.Morph (MFunctor (hoist))
import Control.Monad.Oops (Variant)
import Control.Monad.Oops qualified as Oops
import Control.Tracer (Tracer, debugTracer, nullTracer)
import Data.IntCast (intCast)
import Data.Map.Strict qualified as Map
import GHC.IO.Exception (userError)
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
import Ouroboros.Network.Mux (RunMiniProtocol (..), mkMiniProtocolCbFromPeer)
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
  , networkErrorPolicies
  , withIOManager
  )
import Ouroboros.Network.Protocol.ChainSync.Client (chainSyncClientPeer)
import Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import Ouroboros.Network.Protocol.LocalStateQuery.Client
  ( localStateQueryClientPeer
  )
import Ouroboros.Network.Protocol.LocalStateQuery.Type (LocalStateQuery)
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
  ( localTxSubmissionClientPeer
  )
import String.ANSI (blackBg, faint)
import Text.Pretty.Simple (pPrint)
import Yare.Addresses (Error (..))
import Yare.Addresses qualified as Addresses
import Yare.App.Types qualified as App
import Yare.Chain.Block (HFBlock, IxedByBlock (..))
import Yare.Chain.Follower
  ( ChainFollower (..)
  , ChainState (..)
  , chainTip
  , initialChainState
  , newChainFollower
  )
import Yare.Chain.Point (ChainPoint)
import Yare.Chain.Sync qualified as ChainSync
import Yare.Chain.Types (ChainTip)
import Yare.Http.Server qualified as Http
import Yare.Node.Socket (NodeSocket, nodeSocketLocalAddress)
import Yare.Query (QueryCont (..), queryLedgerTip)
import Yare.Query qualified as Query
import Yare.Storage (Storage (..))
import Yare.Storage qualified as Storage
import Yare.Submitter (TxSubmitCont)
import Yare.Submitter qualified as Submitter
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo
import Yare.Utxo.State (spendableUtxoEntries)

{- |
Starts several threads concurrently:
- HTTP Server, serving a RESTful API.
- Permanent node connection running a few mini-protocols:
  * Chain sync
  * Local state query
  * Local transaction submission
-}
start ∷ App.Config → IO ()
start App.Config {..} = withHandledErrors do
  addresses ← Addresses.deriveFromMnemonic networkMagic mnemonicFile
  queryQ ← liftIO newTQueueIO
  txSubmissionQ ← liftIO newTQueueIO
  storage ← Storage.inMemory <$> newIORef initialChainState
  let chainFollower = newChainFollower addresses storage
  liftIO . runConcurrently . foldMap Concurrently $
    [ runLocalStateQueries queryQ
    , Warp.run (intCast apiHttpPort) . simpleCors . Http.application $
        App.Services
          { serveUtxo = serveUtxo storage
          , serveTip = serveTip storage
          , deployScript = deployScript storage
          }
    , stayConnectedToNode
        nodeSocket
        networkMagic
        chainFollower
        syncFrom
        queryQ
        txSubmissionQ
        <&> absurd
    ]

{- | Run local state queries.that are provided by a queue.

Each queue item contains a query to run as well as a continuation
that is called with the result of the query.
-}
runLocalStateQueries ∷ TQueue IO (QueryCont IO) → IO ()
runLocalStateQueries queryQ = atomically
  . writeTQueue queryQ
  $ QueryCont (hoist handleQueryErrors queryLedgerTip) \case
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

-- | Retrieves the UTXO set from a storage.
serveUtxo ∷ Storage IO ChainState → IO Utxo
serveUtxo storage = do
  s ← readState storage
  pure $ Utxo.fromList (Map.toList (spendableUtxoEntries (utxoState s)))

-- | Retrieves the chain tip from a storage.
serveTip ∷ MonadIO m ⇒ Storage m ChainState → m ChainTip
serveTip storage = chainTip <$> readState storage

-- | Deploys a script on-chain by submitting a transaction.
deployScript ∷ MonadIO m ⇒ Storage m ChainState → m ()
deployScript _storage = do
  putTextLn "deployScript: not implemented"

-- | Connects to a Cardano Node socket and runs Node-to-Client mini-protocols.
stayConnectedToNode
  ∷ NodeSocket
  → NetworkMagic
  → ChainFollower IO
  → Maybe ChainPoint
  → TQueue IO (QueryCont IO)
  → TQueue IO (TxSubmitCont IO)
  → IO Void
stayConnectedToNode
  nodeSocket
  netMagic
  chainFollower
  syncFrom
  queryQ
  txSubmissionQ =
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
        ( makeNodeToClientProtocols
            chainFollower
            syncFrom
            queryQ
            txSubmissionQ
        )
   where
    handshakeTracer
      ∷ Tracer
          IO
          ( WithMuxBearer
              (ConnectionId LocalAddress)
              (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term))
          ) = showTracer "HS_"

    errorPolicyTracer ∷ Tracer IO (WithAddr LocalAddress ErrorPolicyTrace) =
      showTracer "Err"

    subscriptionTracer ∷ Tracer IO (Identity (SubscriptionTrace LocalAddress)) =
      runIdentity >$< showTracer "SUB"

makeNodeToClientProtocols
  ∷ ∀ {ntcAddr}
   . ChainFollower IO
  → Maybe ChainPoint
  → TQueue IO (QueryCont IO)
  → TQueue IO (TxSubmitCont IO)
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
      { localChainSyncProtocol =
          InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer \_context →
            ( chainSyncTracer
            , cChainSyncCodec
            , chainSyncClientPeer $
                ChainSync.client chainFollower (maybeToList syncFrom)
            )
      , localTxSubmissionProtocol =
          InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer \_context →
            ( nullTracer
            , cTxSubmissionCodec
            , localTxSubmissionClientPeer $ Submitter.client submitQ
            )
      , localStateQueryProtocol =
          InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer \_context →
            ( lsqTracer
            , cStateQueryCodec
            , localStateQueryClientPeer $ Query.client queryQ
            )
      , localTxMonitorProtocol =
          InitiatorProtocolOnly $ mkMiniProtocolCbFromPeer \_context →
            ( nullTracer
            , cTxMonitorCodec
            , localTxMonitorPeerNull
            )
      }
   where
    Codecs
      { cChainSyncCodec
      , cTxSubmissionCodec
      , cStateQueryCodec
      , cTxMonitorCodec
      } = clientCodecs codecConfig blockVer n2cVer

    codecConfig ∷ CodecConfig HFBlock =
      let byronEpochSlots = EpochSlots 21600
       in pClientInfoCodecConfig (protocolClientInfoCardano byronEpochSlots)

    chainSyncTracer
      ∷ Tracer
          IO
          ( TraceSendRecv
              ( ChainSync
                  HFBlock
                  (Point HFBlock)
                  (Tip HFBlock)
              )
          ) = nullTracer

    lsqTracer
      ∷ Tracer
          IO
          ( TraceSendRecv
              ( LocalStateQuery
                  HFBlock
                  (Point HFBlock)
                  (Query HFBlock)
              )
          ) =
        showTracer "Query"

--------------------------------------------------------------------------------
-- Tracing ---------------------------------------------------------------------

showTracer ∷ Show a ⇒ String → Tracer IO a
showTracer prefix =
  faint . ((blackBg (" " <> prefix <> " ") <> " ") <>) . show >$< debugTracer

--------------------------------------------------------------------------------
-- Error handling --------------------------------------------------------------

{- | Given an action that may throw errors,
returns an IO action that also handles errors by reporting them before
exiting the process.
-}
withHandledErrors
  ∷ ExceptT (Variant [Addresses.Error, MkMnemonicError 8]) IO a
  → IO a
withHandledErrors =
  crashOnAddressesError
    >>> crashOnMnemonicError
    >>> Oops.runOops

crashOnAddressesError
  ∷ ExceptT (Variant (Addresses.Error : e)) IO a
  → ExceptT (Variant e) IO a
crashOnAddressesError = Oops.catch \(NetworkMagicNoTag magic) →
  crash $ "Failed to determine a network tag for magic: " <> show magic

crashOnMnemonicError
  ∷ ExceptT (Variant (MkMnemonicError 8 : e)) IO a
  → ExceptT (Variant e) IO a
crashOnMnemonicError = Oops.catch \(err ∷ MkMnemonicError 8) →
  crash $ "Failed to parse mnemonic file: " <> show err

handleQueryErrors
  ∷ ExceptT
      ( Variant
          [ EraMismatch
          , Query.UnknownEraIndex
          , Query.NoLedgerTipQueryInByronEra
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
  ∷ ExceptT (Variant (Query.UnknownEraIndex : e)) IO a
  → ExceptT (Variant e) IO a
crashOnUnknownEraIndex = Oops.catch \(err ∷ Query.UnknownEraIndex) →
  crash $ "Local state query unknown era: " <> show err

crashOnNoLedgerTipQuery
  ∷ ExceptT (Variant (Query.NoLedgerTipQueryInByronEra : e)) IO a
  → ExceptT (Variant e) IO a
crashOnNoLedgerTipQuery = Oops.catch \(err ∷ Query.NoLedgerTipQueryInByronEra) →
  crash $ "No ledger tip query in Byron era: " <> show err

crash ∷ MonadIO m ⇒ Text → m a
crash = liftIO . throwIO . userError . toString
