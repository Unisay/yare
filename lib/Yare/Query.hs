{-# LANGUAGE UndecidableInstances #-}

module Yare.Query
  ( Q
  , LsqM
  , QueryCont (..)
  , queryHistoryInterpreter
  , queryCurrentEra
  , queryLedgerTip
  , querySystemStart
  , client
  , submit

    -- * Errors
  , UnknownEraIndex (..)
  , NoLedgerTipQueryInByronEra (..)
  ) where

import Relude hiding (atomically, show)

import Cardano.Ledger.Api (PParams)
import Cardano.Slotting.Time (SystemStart)
import Control.Concurrent.Class.MonadSTM.TQueue (TQueue, readTQueue, writeTQueue)
import Control.Monad (ap, liftM, liftM2)
import Control.Monad.Class.MonadSTM (MonadSTM (atomically))
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Morph (MFunctor (..), hoist)
import Control.Monad.Oops (CouldBe, Variant)
import Control.Monad.Oops qualified as Oops
import Control.Monad.Zip (MonadZip (..))
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
  ( BlockQuery (QueryAnytime, QueryHardFork, QueryIfCurrent)
  , QueryAnytime (..)
  , QueryHardFork (..)
  , QueryIfCurrent (..)
  )
import Ouroboros.Consensus.HardFork.History qualified as History
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Consensus.Shelley.Ledger.Query (BlockQuery (GetCurrentPParams))
import Ouroboros.Consensus.Shelley.Ledger.Query qualified as Shelley
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block (Point)
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as Query
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import Ouroboros.Network.Protocol.LocalStateQuery.Type qualified as Query
import Text.Show (show)
import Yare.Chain.Block (Blocks, HFBlock, IxedByBlock (..))
import Yare.Chain.Era (Era (..))
import Ouroboros.Consensus.Ledger.Query qualified as Consensus

type Q ∷ Type
type Q = TQueue IO (QueryCont IO)

type QueryCont ∷ (Type → Type) → Type
data QueryCont m = ∀ r. QueryCont (LsqM m r) (Either AcquireFailure r → m ())

client
  ∷ Q → Query.LocalStateQueryClient HFBlock (Point HFBlock) (Query HFBlock) IO a
client queryQ = Query.LocalStateQueryClient idleState
 where
  idleState
    ∷ IO
        ( Query.ClientStIdle
            HFBlock
            (Point HFBlock)
            (Query HFBlock)
            IO
            a
        )
  idleState = Query.SendMsgAcquire Query.VolatileTip <$> acquiringState

  acquiringState
    ∷ IO
        ( Query.ClientStAcquiring
            HFBlock
            (Point HFBlock)
            (Query HFBlock)
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
            HFBlock
            (Point HFBlock)
            (Query HFBlock)
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
                HFBlock
                (Point HFBlock)
                (Query HFBlock)
                IO
                a
            )
      )
    → IO
        ( Query.ClientStAcquired
            HFBlock
            (Point HFBlock)
            (Query HFBlock)
            IO
            a
        )
  evalLsq lsq k =
    case lsq of
      LsqLift r → k =<< r
      LsqBind l f → evalLsq l \r → evalLsq (f r) k
      LsqQuery q → pure do
        Query.SendMsgQuery q Query.ClientStQuerying {recvMsgResult = k}

-- | Local State Query Monad allows composing local state queries
type LsqM ∷ (Type → Type) → Type → Type
data LsqM m a
  = LsqQuery (Consensus.Query HFBlock a)
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

--------------------------------------------------------------------------------
-- Queries ---------------------------------------------------------------------

querySystemStart ∷ LsqM m SystemStart
querySystemStart = LsqQuery GetSystemStart

queryHistoryInterpreter ∷ LsqM m (History.Interpreter Blocks)
queryHistoryInterpreter = LsqQuery (BlockQuery (QueryHardFork GetInterpreter))

queryCurrentEraIndex ∷ LsqM m (EraIndex Blocks)
queryCurrentEraIndex = LsqQuery (BlockQuery (QueryHardFork GetCurrentEra))

queryCurrentPParams ∷ Monad m ⇒ LsqM m (PParams era)
queryCurrentPParams = undefined

queryCurrentEra
  ∷ (MonadError (Variant e) m, e `CouldBe` UnknownEraIndex)
  ⇒ LsqM m Era
queryCurrentEra =
  LsqQuery (BlockQuery (QueryHardFork GetCurrentEra)) >>= \idx →
    case eraIndexToInt idx of
      0 → pure Byron
      1 → pure Shelley
      2 → pure Allegra
      3 → pure Mary
      4 → pure Alonzo
      5 → pure Babbage
      6 → pure Conway
      _ → lift $ Oops.throw (UnknownEraIndex idx)

queryLedgerTip
  ∷ ∀ m e
   . ( MonadError (Variant e) m
     , e `CouldBe` UnknownEraIndex
     , e `CouldBe` EraMismatch
     , e `CouldBe` NoLedgerTipQueryInByronEra
     )
  ⇒ LsqM m (IxedByBlock Point)
queryLedgerTip =
  queryCurrentEra >>= \case
    Byron →
      lift $ Oops.throw NoLedgerTipQueryInByronEra
    Shelley →
      IxedByBlockShelley <$> query (QueryIfCurrentShelley Shelley.GetLedgerTip)
    Allegra →
      IxedByBlockAllegra <$> query (QueryIfCurrentAllegra Shelley.GetLedgerTip)
    Mary →
      IxedByBlockMary <$> query (QueryIfCurrentMary Shelley.GetLedgerTip)
    Alonzo →
      IxedByBlockAlonzo <$> query (QueryIfCurrentAlonzo Shelley.GetLedgerTip)
    Babbage →
      IxedByBlockBabbage <$> query (QueryIfCurrentBabbage Shelley.GetLedgerTip)
    Conway → do
      IxedByBlockConway <$> query (QueryIfCurrentConway Shelley.GetLedgerTip)
 where
  query ∷ BlockQuery HFBlock (CardanoQueryResult StandardCrypto a) → LsqM m a
  query q =
    LsqQuery (BlockQuery q) >>= \case
      QueryResultEraMismatch eraMismatch → lift $ Oops.throw eraMismatch
      QueryResultSuccess res → pure res

--------------------------------------------------------------------------------
-- Submission ------------------------------------------------------------------

submit ∷ Q → LsqM IO a → IO (Either AcquireFailure a)
submit queryQ lsq = do
  var ← newEmptyMVar
  atomically . writeTQueue queryQ $
    QueryCont lsq \case
      Left acquireFailure → putMVar var (Left acquireFailure)
      Right result → putMVar var (Right result)
  takeMVar var

--------------------------------------------------------------------------------
-- Errors ----------------------------------------------------------------------

type UnknownEraIndex ∷ Type
newtype UnknownEraIndex = UnknownEraIndex (EraIndex Blocks)
  deriving stock (Eq)

instance Show UnknownEraIndex where
  show (UnknownEraIndex idx) = "Unknown era index: " <> show (eraIndexToInt idx)

type NoLedgerTipQueryInByronEra ∷ Type
data NoLedgerTipQueryInByronEra = NoLedgerTipQueryInByronEra
  deriving stock (Eq, Show)
