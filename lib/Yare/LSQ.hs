{-# LANGUAGE UndecidableInstances #-}

module Yare.LSQ
  ( LsqM
  , QueryCont (..)
  , queryCurrentEra
  , queryLedgerTip
  , localStateQueryClient

    -- * Errors
  , UnknownEraIndex (..)
  , NoLedgerTipQueryInByronEra (..)
  ) where

import Relude hiding (atomically, show)

import Control.Concurrent.Class.MonadSTM.TQueue (TQueue, readTQueue)
import Control.Monad (ap, liftM)
import Control.Monad.Class.MonadSTM (MonadSTM (atomically))
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Morph (MFunctor (..), hoist)
import Control.Monad.Oops (CouldBe, Variant)
import Control.Monad.Oops qualified as Oops
import Ouroboros.Consensus.Cardano.Block
  ( BlockQuery (..)
  , CardanoQueryResult
  , Either (..)
  , EraMismatch (..)
  , HardForkBlock
  , StandardCrypto
  )
import Ouroboros.Consensus.HardFork.Combinator.Abstract (EraIndex, eraIndexToInt)
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query
  ( QueryHardFork (GetCurrentEra)
  )
import Ouroboros.Consensus.Ledger.Query (Query (..))
import Ouroboros.Consensus.Protocol.Praos.Translate ()
import Ouroboros.Consensus.Shelley.Ledger.Query qualified as Shelley
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block (Point)
import Ouroboros.Network.Protocol.LocalStateQuery.Client qualified as LSQ
import Ouroboros.Network.Protocol.LocalStateQuery.Type qualified as LSQ
import Text.Show (show)
import Yare.Chain.Block (Blocks, HFBlock, IxedByBlock (..))
import Yare.Chain.Era (Era (..))

localStateQueryClient
  ∷ ∀ m a
   . MonadSTM m
  ⇒ TQueue m (QueryCont m)
  → LSQ.LocalStateQueryClient HFBlock (Point HFBlock) (Query HFBlock) m a
localStateQueryClient queryQ = LSQ.LocalStateQueryClient idleState
 where
  idleState ∷ m (LSQ.ClientStIdle HFBlock (Point HFBlock) (Query HFBlock) m a)
  idleState = LSQ.SendMsgAcquire Nothing <$> acquiringState

  acquiringState
    ∷ m (LSQ.ClientStAcquiring HFBlock (Point HFBlock) (Query HFBlock) m a)
  acquiringState =
    atomically (readTQueue queryQ) <&> \(QueryCont lsq k) →
      LSQ.ClientStAcquiring
        { recvMsgAcquired = acquiredState lsq (k . Right)
        , recvMsgFailure = \failure → k (Left failure) *> idleState
        }

  acquiredState
    ∷ LsqM m r
    → (r → m ())
    → m (LSQ.ClientStAcquired HFBlock (Point HFBlock) (Query HFBlock) m a)
  acquiredState lsq respond =
    evalLsq lsq \r → pure (LSQ.SendMsgRelease (respond r *> idleState))

  evalLsq
    ∷ LsqM m r
    → (r → m (LSQ.ClientStAcquired HFBlock (Point HFBlock) (Query HFBlock) m a))
    → m (LSQ.ClientStAcquired HFBlock (Point HFBlock) (Query HFBlock) m a)
  evalLsq lsq k =
    case lsq of
      LsqLift r → k =<< r
      LsqBind l f → evalLsq l \r → evalLsq (f r) k
      LsqQuery q → pure do
        LSQ.SendMsgQuery q LSQ.ClientStQuerying {recvMsgResult = k}

data LsqM (m ∷ Type → Type) a
  = LsqQuery (Query (HardForkBlock Blocks) a)
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

data QueryCont m
  = ∀ r. QueryCont (LsqM m r) (Either LSQ.AcquireFailure r → m ())


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
-- Errors ----------------------------------------------------------------------

newtype UnknownEraIndex = UnknownEraIndex (EraIndex Blocks)
  deriving stock (Eq)

instance Show UnknownEraIndex where
  show (UnknownEraIndex idx) = "Unknown era index: " <> show (eraIndexToInt idx)

data NoLedgerTipQueryInByronEra = NoLedgerTipQueryInByronEra
  deriving stock (Eq, Show)
