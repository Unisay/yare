module Yare.Http.Server.Start (start) where

import Yare.Prelude hiding (atomically)

import Cardano.Api.Shelley
  ( AnyShelleyBasedEra (..)
  , BabbageEraOnwards (BabbageEraOnwardsBabbage, BabbageEraOnwardsConway)
  , EraHistory (..)
  , NetworkMagic
  , ShelleyBasedEra (..)
  , babbageEraOnwardsToShelleyBasedEra
  , toLedgerEpochInfo
  )
import Codec.Serialise.Class.Orphans ()
import Control.Exception (throwIO)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Oops (Variant)
import Control.Monad.Oops qualified as Oops
import Fmt.Orphans ()
import GHC.IO.Exception (userError)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Ouroboros.Consensus.Cardano.Block (EraMismatch)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Yare.Address (Addresses)
import Yare.Address qualified as Address
import Yare.Address qualified as Addresses
import Yare.App.Services qualified as App
import Yare.App.State qualified as Yare
import Yare.App.Types (NetworkInfo (..))
import Yare.App.Types qualified as Yare
import Yare.Http.Server qualified as Http
import Yare.Query qualified as Query
import Yare.Storage (Storage (..))
import Yare.Submitter qualified as Submitter

-- | Runs a web server serving web application via a RESTful API.
start
  ∷ ∀ envᵣ
   . ( Yare.Configᵣ ∈∈ HList envᵣ
     , [Query.Q, Submitter.Q, Addresses, Storage IO Yare.State] ∈∈ HList envᵣ
     )
  ⇒ HList envᵣ
  → IO ()
start env = withHandledErrors do
  Query.submit (look @Query.Q env) Query.queryCurrentShelleyEra
    >>= Oops.hoistMaybe UnsupportedEraByron
    >>= \sbe@(AnyShelleyBasedEra (shelleyBasedEra ∷ ShelleyBasedEra era)) → do
      let unsupportedEra ∷ ExceptT Errors IO a
          unsupportedEra = Oops.throw (UnsupportedEraShelley sbe)
      case shelleyBasedEra of
        ShelleyBasedEraShelley → unsupportedEra
        ShelleyBasedEraAllegra → unsupportedEra
        ShelleyBasedEraMary → unsupportedEra
        ShelleyBasedEraAlonzo → unsupportedEra
        ShelleyBasedEraBabbage →
          withBabbageEraOnwards @era BabbageEraOnwardsBabbage
        ShelleyBasedEraConway →
          withBabbageEraOnwards @era BabbageEraOnwardsConway
 where
  withBabbageEraOnwards ∷ ∀ era. BabbageEraOnwards era → ExceptT Errors IO ()
  withBabbageEraOnwards currentEra = do
    -- Making NetworkInfo ------------------------------------------------
    network ← Oops.hoistEither do
      Addresses.networkMagicToLedgerNetwork (look @NetworkMagic env)
    (systemStart, historyInterpreter, errorOrProtocolParams) ←
      Query.submit (look @Query.Q env) $
        (,,)
          <$> Query.querySystemStart
          <*> Query.queryHistoryInterpreter
          <*> Query.queryCurrentPParams
            (babbageEraOnwardsToShelleyBasedEra currentEra)
    protocolParameters ←
      case errorOrProtocolParams of
        Left err → throwError err
        Right ledgerProtocolParameters → pure ledgerProtocolParameters

    let envWithNetworkInfo ∷ HList (NetworkInfo era ': envᵣ) =
          HCons
            NetworkInfo
              { network
              , systemStart
              , currentEra
              , epochInfo = toLedgerEpochInfo (EraHistory historyInterpreter)
              , protocolParameters
              }
            env

    -- Running the server ------------------------------------------------
    liftIO
      . Warp.run (look @Warp.Port env)
      . simpleCors
      . Http.application
      $ App.mkServices @era @Yare.State envWithNetworkInfo

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
