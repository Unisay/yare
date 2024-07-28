-- | Description: Yare application entry point
module Yare.App (start) where

import Relude hiding (atomically)

import Cardano.Api.Shelley
  ( AnyShelleyBasedEra (..)
  , EraHistory (..)
  , ShelleyBasedEra (..)
  , toLedgerEpochInfo
  )
import Cardano.Client.Subscription (subscribe)
import Cardano.Mnemonic (MkMnemonicError)
import Control.Concurrent.Class.MonadSTM.TQueue (newTQueueIO)
import Control.Exception (throwIO)
import Control.Monad.Class.MonadAsync (concurrently_)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Oops (Variant)
import Control.Monad.Oops qualified as Oops
import Data.IORef.Strict qualified as Strict
import GHC.IO.Exception (userError)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Ouroboros.Consensus.Cardano.Block (EraMismatch)
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
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Yare.Addresses
  ( AddressConversionError (..)
  , AddressDerivationError (..)
  , Addresses
  , NetworkMagicNoTagError (..)
  )
import Yare.Addresses qualified as Addresses
import Yare.App.Services qualified as App
import Yare.App.Types
  ( AppState
  , Config (apiHttpPort)
  , NetworkInfo (..)
  , chainState
  )
import Yare.App.Types qualified as App
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Follower (ChainState (..), newChainFollower)
import Yare.Http.Server qualified as Http
import Yare.Node.Protocols (makeNodeToClientProtocols)
import Yare.Node.Socket (nodeSocketLocalAddress)
import Yare.Query qualified as Query
import Yare.Storage (Storage (..), zoomStorage)
import Yare.Storage qualified as Storage
import Yare.Submitter qualified as Submitter
import Yare.Tracer (nullTracer, showTracer)

{- |
Starts several threads concurrently:
- HTTP Server, serving a RESTful API.
- Permanent node connection running a few mini-protocols:
  * Chain sync
  * Local state query
  * Local transaction submission
-}
start ∷ App.Config → IO ()
start config@App.Config {networkMagic, mnemonicFile} = do
  addresses ← withHandledErrors do
    Addresses.deriveFromMnemonic networkMagic mnemonicFile
  queryQ ← liftIO newTQueueIO
  submitQ ← liftIO newTQueueIO
  let !appState = App.initialState addresses
  storage ← Storage.inMemory <$> Strict.newIORef appState
  concurrently_
    ( runWebServer
        config
        storage
        queryQ
        submitQ
    )
    ( runNodeConnection
        config
        addresses
        (zoomStorage chainState storage)
        queryQ
        submitQ
    )

-- | Runs a web server serving web application via a RESTful API.
runWebServer
  ∷ Config
  -- ^ Application configuration
  → Storage IO AppState
  -- ^ Storage for the chain state
  → Query.Q
  -- ^ A queue used to send local state query requests
  → Submitter.Q
  -- ^ A queue used to send transaction submission requests
  → IO ()
runWebServer App.Config {networkMagic, apiHttpPort} storage queryQ submitQ =
  withHandledErrors do
    Query.submit queryQ Query.queryCurrentShelleyEra
      >>= Oops.hoistMaybe UnsupportedEraByron
      >>= \case
        AnyShelleyBasedEra (currentEra ∷ ShelleyBasedEra era) → do
          -- Making NetworkInfo ------------------------------------------------
          network ← Addresses.networkMagicToLedgerNetwork networkMagic
          (systemStart, historyInterpreter, errorOrProtocolParams) ←
            Query.submit queryQ $
              (,,)
                <$> Query.querySystemStart
                <*> Query.queryHistoryInterpreter
                <*> Query.queryCurrentPParams currentEra
          let epochInfo = toLedgerEpochInfo (EraHistory historyInterpreter)
          protocolParameters ← either throwError pure errorOrProtocolParams
          let networkInfo =
                NetworkInfo
                  { network
                  , systemStart
                  , currentEra
                  , epochInfo
                  , protocolParameters
                  }
          -- Running the server ------------------------------------------------
          liftIO . Warp.run apiHttpPort . simpleCors . Http.application $
            App.mkServices storage submitQ networkInfo

--------------------------------------------------------------------------------
-- Node connection -------------------------------------------------------------

-- | Connects to a Cardano Node socket and runs Node-to-Client mini-protocols.
runNodeConnection
  ∷ App.Config
  → Addresses
  → Storage IO ChainState
  → Query.Q
  → Submitter.Q
  → IO Void
runNodeConnection App.Config {..} addresses storage queryQ submitQ = do
  let chainFollower = newChainFollower addresses storage
  withIOManager \ioManager →
    subscribe
      (localSnocket ioManager)
      networkMagic
      (supportedNodeToClientVersions (Proxy @StdCardanoBlock))
      NetworkSubscriptionTracers
        { nsMuxTracer = nullTracer
        , nsHandshakeTracer = showTracer "HS_"
        , nsErrorPolicyTracer = showTracer "Err"
        , nsSubscriptionTracer = runIdentity >$< showTracer "SUB"
        }
      ClientSubscriptionParams
        { cspAddress = nodeSocketLocalAddress nodeSocket
        , cspConnectionAttemptDelay = Nothing
        , cspErrorPolicies =
            networkErrorPolicies <> consensusErrorPolicy (Proxy @StdCardanoBlock)
        }
      ( makeNodeToClientProtocols
          chainFollower
          syncFrom
          queryQ
          submitQ
      )

--------------------------------------------------------------------------------
-- Error handling --------------------------------------------------------------

type UnsupportedEraByron ∷ Type
data UnsupportedEraByron = UnsupportedEraByron
  deriving stock (Show)

type InvalidTxIdHash ∷ Type
newtype InvalidTxIdHash = InvalidTxIdHash ByteString
  deriving stock (Show)

type Errors ∷ Type
type Errors =
  Variant
    [ UnsupportedEraByron
    , NetworkMagicNoTagError
    , AddressConversionError
    , AddressDerivationError
    , MkMnemonicError 8
    , AcquireFailure
    , EraMismatch
    , Query.NoQueryInByronEra
    , InvalidTxIdHash
    ]

{- | Given an action that may throw errors,
returns an IO action that also handles errors by reporting them before
exiting the process.
-}
withHandledErrors ∷ ExceptT Errors IO a → IO a
withHandledErrors =
  crashOnUnsupportedEraError
    >>> crashOnNetworkMagicNoTagError
    >>> crashOnAddressConversionError
    >>> crashOnAddressDerivationError
    >>> crashOnMnemonicError
    >>> crashOnAcquireFailure
    >>> crashOnEraMismatch
    >>> crashOnNoQuery
    >>> crashOnInvalidTxIdHash
    >>> Oops.runOops

crashOnUnsupportedEraError
  ∷ ExceptT (Variant (UnsupportedEraByron : e)) IO a
  → ExceptT (Variant e) IO a
crashOnUnsupportedEraError = Oops.catch \UnsupportedEraByron →
  crash "Current node era (Byron) is not supported."

crashOnNetworkMagicNoTagError
  ∷ ExceptT (Variant (NetworkMagicNoTagError : e)) IO a
  → ExceptT (Variant e) IO a
crashOnNetworkMagicNoTagError = Oops.catch \(NetworkMagicNoTag magic) →
  crash $ "Failed to determine a network tag for magic: " <> show magic

crashOnAddressConversionError
  ∷ ExceptT (Variant (AddressConversionError : e)) IO a
  → ExceptT (Variant e) IO a
crashOnAddressConversionError = Oops.catch \(AddressConversionError addr) →
  crash $ "Failed to convert address: " <> show addr

crashOnAddressDerivationError
  ∷ ExceptT (Variant (AddressDerivationError : e)) IO a
  → ExceptT (Variant e) IO a
crashOnAddressDerivationError = Oops.catch \NoAddressesDerived →
  crash "Failed to derive addresses from mnemonic"

crashOnMnemonicError
  ∷ ExceptT (Variant (MkMnemonicError 8 : e)) IO a
  → ExceptT (Variant e) IO a
crashOnMnemonicError = Oops.catch \(err ∷ MkMnemonicError 8) →
  crash $ "Failed to parse mnemonic file: " <> show err

crashOnAcquireFailure
  ∷ ExceptT (Variant (AcquireFailure : e)) IO a
  → ExceptT (Variant e) IO a
crashOnAcquireFailure = Oops.catch \(err ∷ AcquireFailure) →
  crash $ "Failed to acquire local node state for querying: " <> show err

crashOnEraMismatch
  ∷ ExceptT (Variant (EraMismatch : e)) IO a
  → ExceptT (Variant e) IO a
crashOnEraMismatch = Oops.catch \(err ∷ EraMismatch) →
  crash $ "Era mismatch: " <> show err

crashOnNoQuery
  ∷ ExceptT (Variant (Query.NoQueryInByronEra : e)) IO a
  → ExceptT (Variant e) IO a
crashOnNoQuery = Oops.catch \(Query.NoQueryInByronEra query) →
  crash . unwords $
    [ "Application attempted to send a local state query "
    , show query
    , "but the Cardano Node is currently in the Byron "
    , "era and such query is not available."
    ]

crashOnInvalidTxIdHash
  ∷ ExceptT (Variant (InvalidTxIdHash : e)) IO a
  → ExceptT (Variant e) IO a
crashOnInvalidTxIdHash = Oops.catch \(InvalidTxIdHash hash) →
  crash $ "Failed to parse transaction ID hash: " <> show hash

crash ∷ MonadIO m ⇒ Text → m a
crash = liftIO . throwIO . userError . toString
