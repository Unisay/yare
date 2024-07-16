-- | Description: Yare application entry point
module Yare.App (start) where

import Relude hiding (atomically)

import Cardano.Api (InAnyShelleyBasedEra (..))
import Cardano.Api.Ledger (KeyRole (DRepRole))
import Cardano.Api.Shelley
  ( PoolId
  , StakeCredential
  , TxBodyErrorAutoBalance
  , inAnyShelleyBasedEra
  )
import Cardano.Api.Shelley qualified as Api
import Cardano.Client.Subscription (subscribe)
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Credential (Credential)
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Mnemonic (MkMnemonicError)
import Control.Concurrent.Class.MonadSTM.TQueue (newTQueueIO)
import Control.Exception (throwIO)
import Control.Monad.Class.MonadAsync (concurrently_)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Oops (Variant)
import Control.Monad.Oops qualified as Oops
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Variant qualified as Variant
import GHC.IO.Exception (userError)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Ouroboros.Consensus.Cardano.Block (CardanoApplyTxErr, EraMismatch)
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
import Yare.Addresses (Error (..))
import Yare.Addresses qualified as Addresses
import Yare.App.Types (Config (apiHttpPort), NetworkInfo (..))
import Yare.App.Types qualified as App
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Follower
  ( ChainState (..)
  , chainTip
  , initialChainState
  , newChainFollower
  )
import Yare.Chain.Types (ChainTip)
import Yare.Http.Server qualified as Http
import Yare.Node.Protocols (makeNodeToClientProtocols)
import Yare.Node.Socket (nodeSocketLocalAddress)
import Yare.Query qualified as Query
import Yare.Storage (Storage (..))
import Yare.Storage qualified as Storage
import Yare.Submitter qualified as Submitter
import Yare.Tracer (nullTracer, showTracer)
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
start config@App.Config {apiHttpPort} = do
  queryQ ← liftIO newTQueueIO
  submitQ ← liftIO newTQueueIO
  storage ← Storage.inMemory <$> newIORef initialChainState
  concurrently_
    (runWebServer apiHttpPort storage queryQ submitQ)
    (runNodeConnection config storage queryQ submitQ)

-- | Retrieves the UTXO set from a storage.
serveUtxo ∷ Storage IO ChainState → IO Utxo
serveUtxo storage = do
  s ← readState storage
  pure $ Utxo.fromList (Map.toList (spendableUtxoEntries (utxoState s)))

-- | Retrieves the chain tip from a storage.
serveTip ∷ Storage IO ChainState → IO ChainTip
serveTip storage = chainTip <$> readState storage

-- | Deploys a script on-chain by submitting a transaction.
deployScript
  ∷ Submitter.Q
  → NetworkInfo
  → IO
      ( Maybe
          ( Variant
              [ CardanoApplyTxErr StandardCrypto
              , InAnyShelleyBasedEra TxBodyErrorAutoBalance
              ]
          )
      )
deployScript submitQ networkInfo = do
  let NetworkInfo {protocolParameters, epochInfo, systemStart} = networkInfo

  let bodyContent ∷ Api.TxBodyContent Api.BuildTx era
      bodyContent = undefined

  let changeAddr ∷ Api.AddressInEra era
      changeAddr = undefined

  let overrideKeyWitnesses ∷ Maybe Word
      overrideKeyWitnesses = Nothing

  let txInputs ∷ Api.UTxO era
      txInputs = undefined

  let registeredPools ∷ Set PoolId
      registeredPools = Set.empty

  let delegations ∷ Map StakeCredential L.Coin
      delegations = Map.empty

  let delegationsRewards ∷ Map (Credential DRepRole StandardCrypto) L.Coin
      delegationsRewards = Map.empty

  let witnesses ∷ [Api.ShelleyWitnessSigningKey]
      witnesses = []

  case protocolParameters of
    InAnyShelleyBasedEra era protocolParams →
      case Api.constructBalancedTx
        era
        bodyContent
        changeAddr
        overrideKeyWitnesses
        txInputs
        protocolParams
        epochInfo
        systemStart
        registeredPools
        delegations
        delegationsRewards
        witnesses of
        Left err →
          pure (Just (Variant.throw (inAnyShelleyBasedEra era err)))
        Right signedBalancedTx →
          Submitter.submit submitQ (Api.TxInMode era signedBalancedTx)

-- | Runs a web server serving web application via a RESTful API.
runWebServer
  ∷ Warp.Port
  → Storage IO ChainState
  → Query.Q
  → Submitter.Q
  → IO ()
runWebServer httpPort storage queryQ submitQ = withHandledErrors do
  (systemStart, historyInterpreter, errorOrProtocolParams) ←
    Query.submit queryQ $
      (,,)
        <$> Query.querySystemStart
        <*> Query.queryHistoryInterpreter
        <*> Query.queryCurrentPParams

  let eraHistory = Api.EraHistory historyInterpreter

  protocolParameters ← either throwError pure errorOrProtocolParams

  let networkInfo =
        App.NetworkInfo
          { systemStart
          , epochInfo = Api.toLedgerEpochInfo eraHistory
          , protocolParameters
          }

  liftIO $
    Warp.run httpPort . simpleCors . Http.application $
      App.Services
        { serveUtxo = serveUtxo storage
        , serveTip = serveTip storage
        , deployScript = deployScript submitQ networkInfo
        }

-- | Connects to a Cardano Node socket and runs Node-to-Client mini-protocols.
runNodeConnection
  ∷ App.Config
  → Storage IO ChainState
  → Query.Q
  → Submitter.Q
  → IO Void
runNodeConnection App.Config {..} storage queryQ submitQ = do
  addresses ← withHandledErrors do
    Addresses.deriveFromMnemonic networkMagic mnemonicFile
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

type Errors ∷ Type
type Errors =
  Variant
    [ Addresses.Error
    , MkMnemonicError 8
    , AcquireFailure
    , EraMismatch
    , Query.NoQueryInByronEra
    ]

{- | Given an action that may throw errors,
returns an IO action that also handles errors by reporting them before
exiting the process.
-}
withHandledErrors ∷ ExceptT Errors IO a → IO a
withHandledErrors =
  crashOnAddressesError
    >>> crashOnMnemonicError
    >>> crashOnAcquireFailure
    >>> crashOnEraMismatch
    >>> crashOnNoQuery
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

crash ∷ MonadIO m ⇒ Text → m a
crash = liftIO . throwIO . userError . toString
