-- | Description: Yare application entry point
module Yare.App (start) where

import Yare.Prelude hiding (atomically)

import Cardano.Api.Shelley
  ( AnyShelleyBasedEra (..)
  , BabbageEraOnwards (BabbageEraOnwardsBabbage, BabbageEraOnwardsConway)
  , EraHistory (..)
  , ShelleyBasedEra (..)
  , babbageEraOnwardsToShelleyBasedEra
  , toLedgerEpochInfo
  )
import Cardano.Client.Subscription (subscribe)
import Control.Concurrent.Class.MonadSTM.TQueue (newTQueueIO)
import Control.Exception (throwIO)
import Control.Monad.Class.MonadAsync (concurrently_)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Oops (Variant)
import Control.Monad.Oops qualified as Oops
import Data.IORef.Strict qualified as Strict
import Data.Row.Records (Rec, type (≈))
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
import Yare.Address (Addresses)
import Yare.Address qualified as Address
import Yare.Address qualified as Addresses
import Yare.App.Services qualified as App
import Yare.App.State (HasAppState)
import Yare.App.State qualified as Yare
import Yare.App.Types (NetworkInfo (..))
import Yare.App.Types qualified as Yare
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Follower (HasChainState, newChainFollower)
import Yare.Http.Server qualified as Http
import Yare.Node.Protocols (makeNodeToClientProtocols)
import Yare.Node.Socket (nodeSocketLocalAddress)
import Yare.Query qualified as Query
import Yare.Storage (Storage (..))
import Yare.Storage qualified as Storage
import Yare.Submitter qualified as Submitter
import Yare.Tracer (nullTracer, prefixTracer, prefixTracerShow)

{- |
Starts several threads concurrently:
- HTTP Server, serving a RESTful API.
- Permanent node connection running a few mini-protocols:
  * Chain sync
  * Local state query
  * Local transaction submission
-}
start ∷ Yare.Config → IO ()
start config@Yare.Config {networkMagic, mnemonicFile} = do
  addresses ←
    Addresses.deriveFromMnemonic networkMagic mnemonicFile
      & Oops.onLeftThrow
      & withHandledErrors
  queryQ ← liftIO newTQueueIO
  submitQ ← liftIO newTQueueIO
  let !appState = Yare.initialState addresses
  storage ← Storage.inMemory <$> Strict.newIORef appState
  concurrently_
    (runWebServer config storage queryQ submitQ)
    (runNodeConnection config storage queryQ submitQ)

-- | Runs a web server serving web application via a RESTful API.
runWebServer
  ∷ Yare.Config
  -- ^ Application configuration
  → Storage IO Yare.State
  -- ^ Storage for the chain state
  → Query.Q
  -- ^ A queue used to send local state query requests
  → Submitter.Q
  -- ^ A queue used to send transaction submission requests
  → IO ()
runWebServer Yare.Config {networkMagic, apiHttpPort} storage queryQ submitQ =
  withHandledErrors do
    Query.submit queryQ Query.queryCurrentShelleyEra
      >>= Oops.hoistMaybe UnsupportedEraByron
      >>= \sbe@(AnyShelleyBasedEra (shelleyBasedEra ∷ ShelleyBasedEra era)) →
        let unsupportedEra ∷ ExceptT Errors IO a
            unsupportedEra = Oops.throw (UnsupportedEraShelley sbe)
         in case shelleyBasedEra of
              ShelleyBasedEraShelley → unsupportedEra
              ShelleyBasedEraAllegra → unsupportedEra
              ShelleyBasedEraMary → unsupportedEra
              ShelleyBasedEraAlonzo → unsupportedEra
              ShelleyBasedEraBabbage →
                withBabbageEraOnwards BabbageEraOnwardsBabbage
              ShelleyBasedEraConway →
                withBabbageEraOnwards BabbageEraOnwardsConway
 where
  withBabbageEraOnwards ∷ BabbageEraOnwards era → ExceptT Errors IO ()
  withBabbageEraOnwards currentEra = do
    let shelleyBasedEra = babbageEraOnwardsToShelleyBasedEra currentEra
    -- Making NetworkInfo ------------------------------------------------
    network ←
      Addresses.networkMagicToLedgerNetwork networkMagic
        & Oops.hoistEither
    (systemStart, historyInterpreter, errorOrProtocolParams) ←
      Query.submit queryQ $
        (,,)
          <$> Query.querySystemStart
          <*> Query.queryHistoryInterpreter
          <*> Query.queryCurrentPParams shelleyBasedEra
    protocolParameters ←
      case errorOrProtocolParams of
        Left err → throwError err
        Right ledgerProtocolParameters → pure ledgerProtocolParameters
    let networkInfo =
          NetworkInfo
            { network
            , systemStart
            , currentEra
            , epochInfo = toLedgerEpochInfo (EraHistory historyInterpreter)
            , protocolParameters
            }
    -- Running the server ------------------------------------------------

    liftIO
      . Warp.run apiHttpPort
      . simpleCors
      . Http.application (prefixTracer "HTTP")
      $ App.mkServices storage submitQ networkInfo

--------------------------------------------------------------------------------
-- Node connection -------------------------------------------------------------

-- | Connects to a Cardano Node socket and runs Node-to-Client mini-protocols.
runNodeConnection
  ∷ ∀ r state
   . (HasAppState r, Rec r ≈ state)
  ⇒ Yare.Config
  → Storage IO state
  → Query.Q
  → Submitter.Q
  → IO Void
runNodeConnection Yare.Config {..} storage queryQ submitQ = do
  let chainFollower = newChainFollower storage
  withIOManager \ioManager →
    subscribe
      (localSnocket ioManager)
      networkMagic
      (supportedNodeToClientVersions (Proxy @StdCardanoBlock))
      NetworkSubscriptionTracers
        { nsMuxTracer = nullTracer
        , nsHandshakeTracer = prefixTracerShow "HS_"
        , nsErrorPolicyTracer = prefixTracerShow "Err"
        , nsSubscriptionTracer = runIdentity >$< prefixTracerShow "SUB"
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

data UnsupportedEra
  = UnsupportedEraByron
  | UnsupportedEraShelley AnyShelleyBasedEra
  deriving stock (Show)

newtype InvalidTxIdHash = InvalidTxIdHash ByteString
  deriving stock (Show)

type Errors =
  Variant
    [ UnsupportedEra
    , Address.Error
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
    >>> crashOnAddressError
    >>> crashOnAcquireFailure
    >>> crashOnEraMismatch
    >>> crashOnNoQuery
    >>> crashOnInvalidTxIdHash
    >>> Oops.runOops

crashOnUnsupportedEraError
  ∷ ExceptT (Variant (UnsupportedEra : e)) IO a
  → ExceptT (Variant e) IO a
crashOnUnsupportedEraError = Oops.catch \case
  UnsupportedEraByron → crash "Current node era (Byron) is not supported."
  UnsupportedEraShelley sbe →
    crash $ "Current node era (Shelley) is not supported: " <> show sbe

crashOnAddressError
  ∷ ExceptT (Variant (Address.Error : e)) IO a
  → ExceptT (Variant e) IO a
crashOnAddressError = Oops.catch \case
  Address.NetworkMagicNoTag magic →
    crash $ "Failed to determine a network tag for magic: " <> show magic
  Address.MnemonicError err →
    crash $ "Failed to parse mnemonic file: " <> show err
  Address.DerivationError err →
    crash $ show err
  Address.NoAddressesDerived →
    crash "Failed to derive addresses from mnemonic"

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
