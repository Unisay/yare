{-# LANGUAGE UndecidableInstances #-}

module Yare.Query
  ( Q
  , LsqM
  , QueryCont (..)
  , client
  , submit

    -- * Queries
  , queryHistoryInterpreter
  , queryCurrentEra
  , queryCurrentShelleyEra
  , queryCurrentPParams
  , queryLedgerTip
  , querySystemStart

    -- * Errors
  , NoQueryInByronEra (..)
  ) where

import Relude hiding (atomically, show)

import Cardano.Api.Shelley
  ( AnyCardanoEra (..)
  , AnyShelleyBasedEra (AnyShelleyBasedEra)
  , CardanoEra (..)
  , InAnyShelleyBasedEra
  , LedgerProtocolParameters (..)
  , ShelleyBasedEra (..)
  , inAnyShelleyBasedEra
  )
import Cardano.Slotting.Time (SystemStart)
import Control.Concurrent.Class.MonadSTM.TQueue
  ( TQueue
  , readTQueue
  , writeTQueue
  )
import Control.Monad (ap, liftM, liftM2)
import Control.Monad.Class.MonadSTM (MonadSTM (atomically))
import Control.Monad.Morph (MFunctor (..), hoist)
import Control.Monad.Oops (CouldBe, Variant)
import Control.Monad.Zip (MonadZip (..))
import Data.Variant qualified as Variant
import Ouroboros.Consensus.Cardano.Block
  ( BlockQuery (..)
  , CardanoQueryResult
  , Either (..)
  , EraMismatch (..)
  , StandardCrypto
  )
import Ouroboros.Consensus.HardFork.Combinator.Abstract
  ( EraIndex
  , eraIndexToInt
  )
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
  ( QueryHardFork (..)
  )
import Ouroboros.Consensus.HardFork.History qualified as History
import Ouroboros.Consensus.Ledger.Query (Query (BlockQuery, GetSystemStart))
import Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import Ouroboros.Consensus.Shelley.Ledger.Query (BlockQuery (GetCurrentPParams))
import Ouroboros.Consensus.Shelley.Ledger.Query qualified as Shelley
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block (Point)
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Query
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Ouroboros.Network.Protocol.LocalStateQuery.Type qualified as Query
import Yare.Chain.Block (Blocks, IxedByBlock (..), StdCardanoBlock)

type Q ∷ Type
type Q = TQueue IO (QueryCont IO)

type QueryCont ∷ (Type → Type) → Type
data QueryCont m = ∀ r. QueryCont (LsqM m r) (Either AcquireFailure r → m ())

client
  ∷ Q
  → Query.LocalStateQueryClient
      StdCardanoBlock
      (Point StdCardanoBlock)
      (Query StdCardanoBlock)
      IO
      a
client queryQ = Query.LocalStateQueryClient idleState
 where
  idleState
    ∷ IO
        ( Query.ClientStIdle
            StdCardanoBlock
            (Point StdCardanoBlock)
            (Query StdCardanoBlock)
            IO
            a
        )
  idleState = Query.SendMsgAcquire Query.VolatileTip <$> acquiringState

  acquiringState
    ∷ IO
        ( Query.ClientStAcquiring
            StdCardanoBlock
            (Point StdCardanoBlock)
            (Query StdCardanoBlock)
            IO
            a
        )
  acquiringState =
    atomically (readTQueue queryQ) <&> \(QueryCont lsq k) →
      Query.ClientStAcquiring
        { recvMsgAcquired = acquiredState lsq (k . Right)
        , recvMsgFailure = \failure → k (Left failure) *> idleState
        }

  acquiredState
    ∷ LsqM IO r
    → (r → IO ())
    → IO
        ( Query.ClientStAcquired
            StdCardanoBlock
            (Point StdCardanoBlock)
            (Query StdCardanoBlock)
            IO
            a
        )
  acquiredState lsq respond =
    evalLsq lsq \r → pure (Query.SendMsgRelease (respond r *> idleState))

  evalLsq
    ∷ LsqM IO r
    → ( r
        → IO
            ( Query.ClientStAcquired
                StdCardanoBlock
                (Point StdCardanoBlock)
                (Query StdCardanoBlock)
                IO
                a
            )
      )
    → IO
        ( Query.ClientStAcquired
            StdCardanoBlock
            (Point StdCardanoBlock)
            (Query StdCardanoBlock)
            IO
            a
        )
  evalLsq lsq k =
    case lsq of
      LsqLift r → k =<< r
      LsqBind l f → evalLsq l \r → evalLsq (f r) k
      LsqQuery q → pure do
        Query.SendMsgQuery q Query.ClientStQuerying {recvMsgResult = k}

--------------------------------------------------------------------------------
-- Local State Query Monad -----------------------------------------------------

-- | Local State Query Monad allows composing local state queries
type LsqM ∷ (Type → Type) → Type → Type
data LsqM m a
  = LsqQuery (Query StdCardanoBlock a)
  | LsqLift (m a)
  | ∀ r. LsqBind (LsqM m r) (r → LsqM m a)

instance Monad m ⇒ Functor (LsqM m) where
  fmap = liftM

instance Monad m ⇒ Applicative (LsqM m) where
  pure = lift . pure
  (<*>) = ap

instance Monad m ⇒ Monad (LsqM m) where
  return = pure
  (>>=) = LsqBind

instance MonadTrans LsqM where
  lift = LsqLift

instance MonadIO m ⇒ MonadIO (LsqM m) where
  liftIO = lift . liftIO

instance MFunctor LsqM where
  hoist f = \case
    LsqQuery q → LsqQuery q
    LsqLift m → LsqLift (f m)
    LsqBind l k → LsqBind (hoist f l) (hoist f . k)

instance Monad m ⇒ MonadZip (LsqM m) where
  mzip = liftM2 (,)

fromBlockQuery
  ∷ (Monad m, e `CouldBe` EraMismatch)
  ⇒ BlockQuery StdCardanoBlock (CardanoQueryResult StandardCrypto a)
  → LsqM m (Either (Variant e) a)
fromBlockQuery q =
  LsqQuery (BlockQuery q) <&> \case
    QueryResultEraMismatch eraMismatch → Left (Variant.throw eraMismatch)
    QueryResultSuccess res → Right res

--------------------------------------------------------------------------------
-- Queries ---------------------------------------------------------------------

querySystemStart ∷ LsqM m SystemStart
querySystemStart = LsqQuery GetSystemStart

queryHistoryInterpreter ∷ LsqM m (History.Interpreter Blocks)
queryHistoryInterpreter = LsqQuery (BlockQuery (QueryHardFork GetInterpreter))

queryCurrentEraIndex ∷ LsqM m (EraIndex Blocks)
queryCurrentEraIndex = LsqQuery (BlockQuery (QueryHardFork GetCurrentEra))

queryCurrentEra ∷ Monad m ⇒ LsqM m AnyCardanoEra
queryCurrentEra = toEnum . eraIndexToInt <$> queryCurrentEraIndex

queryCurrentShelleyEra ∷ Monad m ⇒ LsqM m (Maybe AnyShelleyBasedEra)
queryCurrentShelleyEra =
  queryCurrentEraIndex <&> \idx →
    case eraIndexToInt idx of
      0 → Nothing
      i → Just (toEnum i)

queryCurrentPParams
  ∷ ( Monad m
    , e `CouldBe` EraMismatch
    , e `CouldBe` NoQueryInByronEra
    )
  ⇒ LsqM m (Either (Variant e) (InAnyShelleyBasedEra LedgerProtocolParameters))
queryCurrentPParams =
  queryCurrentShelleyEra >>= \case
    Nothing →
      pure . Left $ Variant.throw $ NoQueryInByronEra "GetCurrentPParams"
    Just (AnyShelleyBasedEra ShelleyBasedEraShelley) →
      inAnyShelleyBasedEra ShelleyBasedEraShelley
        . LedgerProtocolParameters
        <<$>> fromBlockQuery (QueryIfCurrentShelley GetCurrentPParams)
    Just (AnyShelleyBasedEra ShelleyBasedEraAllegra) →
      inAnyShelleyBasedEra ShelleyBasedEraAllegra
        . LedgerProtocolParameters
        <<$>> fromBlockQuery (QueryIfCurrentAllegra GetCurrentPParams)
    Just (AnyShelleyBasedEra ShelleyBasedEraMary) →
      inAnyShelleyBasedEra ShelleyBasedEraMary
        . LedgerProtocolParameters
        <<$>> fromBlockQuery (QueryIfCurrentMary GetCurrentPParams)
    Just (AnyShelleyBasedEra ShelleyBasedEraAlonzo) →
      inAnyShelleyBasedEra ShelleyBasedEraAlonzo
        . LedgerProtocolParameters
        <<$>> fromBlockQuery (QueryIfCurrentAlonzo GetCurrentPParams)
    Just (AnyShelleyBasedEra ShelleyBasedEraBabbage) →
      inAnyShelleyBasedEra ShelleyBasedEraBabbage
        . LedgerProtocolParameters
        <<$>> fromBlockQuery (QueryIfCurrentBabbage GetCurrentPParams)
    Just (AnyShelleyBasedEra ShelleyBasedEraConway) →
      inAnyShelleyBasedEra ShelleyBasedEraConway
        . LedgerProtocolParameters
        <<$>> fromBlockQuery (QueryIfCurrentConway GetCurrentPParams)

queryLedgerTip
  ∷ ∀ m e
   . ( Monad m
     , e `CouldBe` EraMismatch
     , e `CouldBe` NoQueryInByronEra
     )
  ⇒ LsqM m (Either (Variant e) (IxedByBlock Point))
queryLedgerTip =
  queryCurrentEra >>= \case
    AnyCardanoEra ByronEra →
      pure . Left $ Variant.throw $ NoQueryInByronEra "GetLedgerTip"
    AnyCardanoEra ShelleyEra →
      IxedByBlockShelley <<$>> fromBlockQuery (QueryIfCurrentShelley qry)
    AnyCardanoEra AllegraEra →
      IxedByBlockAllegra <<$>> fromBlockQuery (QueryIfCurrentAllegra qry)
    AnyCardanoEra MaryEra →
      IxedByBlockMary <<$>> fromBlockQuery (QueryIfCurrentMary qry)
    AnyCardanoEra AlonzoEra →
      IxedByBlockAlonzo <<$>> fromBlockQuery (QueryIfCurrentAlonzo qry)
    AnyCardanoEra BabbageEra →
      IxedByBlockBabbage <<$>> fromBlockQuery (QueryIfCurrentBabbage qry)
    AnyCardanoEra ConwayEra →
      IxedByBlockConway <<$>> fromBlockQuery (QueryIfCurrentConway qry)
 where
  qry ∷ BlockQuery (ShelleyBlock proto era) (Point (ShelleyBlock proto era))
  qry = Shelley.GetLedgerTip

--------------------------------------------------------------------------------
-- Submission ------------------------------------------------------------------

submit
  ∷ (e `CouldBe` AcquireFailure, MonadSTM m, MonadIO m)
  ⇒ TQueue m (QueryCont m)
  → LsqM m a
  → ExceptT (Variant e) m a
submit queryQ lsq = do
  var ← newEmptyMVar
  atomically . writeTQueue queryQ $
    QueryCont lsq \case
      Left acquireFailure → putMVar var (Left (Variant.throw acquireFailure))
      Right result → putMVar var (Right result)
  ExceptT $ takeMVar var

--------------------------------------------------------------------------------
-- Errors ----------------------------------------------------------------------

type NoQueryInByronEra ∷ Type
newtype NoQueryInByronEra = NoQueryInByronEra String
  deriving stock (Eq, Show)
