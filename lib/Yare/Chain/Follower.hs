{-# LANGUAGE ImpredicativeTypes #-}

module Yare.Chain.Follower
  ( ChainFollower (..)
  , newChainFollower
  , ChainState
  , ChainStateᵣ
  , initialChainState
  ) where

import Yare.Prelude

import Cardano.Api.Shelley (TxId)
import Cardano.Slotting.Slot (SlotNo, fromWithOrigin)
import Control.Monad.Class.MonadThrow (throwIO)
import Control.Tracer.Extended (Tracer, traceWith)
import Data.Maybe.Strict (StrictMaybe (SJust))
import Data.Set qualified as Set
import Ouroboros.Network.Block
  ( BlockNo (..)
  , Tip (..)
  , blockNo
  , blockSlot
  , getTipBlockNo
  )
import Relude.Extra (dup)
import Yare.Address (Addresses)
import Yare.App.Types (Finality (..), StorageMode (..))
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Block.Reference (blockRef)
import Yare.Chain.Era (AnyEra)
import Yare.Chain.Tx (Tx, transactionViewUtxo, txViewId)
import Yare.Chain.Types (ChainPoint, ChainTip, LastIndexedBlock)
import Yare.Storage (StorageMgr (..), overDefaultStorage, persistVolatileStorage)
import Yare.Tracers (Tracersᵣ)
import Yare.Utxo (Utxo)
import Yare.Utxo qualified as Utxo
import Yare.Utxo.Indexer qualified as Utxo

data ChainFollower (m ∷ Type → Type) = ChainFollower
  { onNewBlock ∷ StdCardanoBlock → ChainTip → m ()
  , onRollback ∷ ChainPoint → ChainTip → m ()
  }

newChainFollower
  ∷ ∀ state env
   . ( Addresses ∈ env
     , StorageMgr IO state ∈ env
     , Tracersᵣ ∈∈ env
     , Utxo ∈ state
     , ChainTip ∈ state
     , LastIndexedBlock ∈ state
     , Tagged "submitted" (Set TxId) ∈ state
     , Tagged "in-ledger" (Set TxId) ∈ state
     )
  ⇒ env
  → ChainFollower IO
newChainFollower env =
  ChainFollower
    { onNewBlock = \(block ∷ StdCardanoBlock) (tip ∷ ChainTip) → do
        let addresses = look @Addresses env
            BlockNo tipBlockNo = fromWithOrigin 0 (getTipBlockNo tip)
            BlockNo thisBlockNo = blockNo block
            finality =
              if thisBlockNo < tipBlockNo - securityParam
                then Final
                else NotFinal

        -- Make a snapshot of the volatile storage every 10_000 blocks
        when (thisBlockNo `rem` 10_000 == 0) do
          persistVolatileStorage @state env

        setStorageMode storageManager case finality of
          -- Once we've reached the volatile (non-final) block,
          -- we switch to the durable storage mode.
          NotFinal → Durable
          -- Otherwise, we stay in the volatile storage mode for
          -- faster indexing.
          Final → Volatile

        let pureStateChange = indexBlock addresses block finality tip

        overDefaultStorage @state storageManager pureStateChange \result → do
          case result of
            Utxo.UtxoNotUpdated →
              traceWith (look @(Tracer IO SlotNo) env) (blockSlot block)
            Utxo.UtxoUpdated updatedUtxo txs → do
              for_ txs $ traceWith (look @(Tracer IO (AnyEra Tx)) env)
              traceWith (look @(Tracer IO Utxo) env) updatedUtxo
            Utxo.UtxoUpdateError err →
              throwIO err
    , onRollback = \(point ∷ ChainPoint) (tip ∷ ChainTip) → do
        traceWith (look @(Tracer IO ChainPoint) env) point
        overDefaultStorage @state
          storageManager
          (dup . rollbackTo point tip)
          (traceWith (look @(Tracer IO Utxo) env) . look @Utxo)
    }
 where
  storageManager = look @(StorageMgr IO state) env

type ChainState = HList ChainStateᵣ
type ChainStateᵣ = [Utxo, ChainTip]

initialChainState ∷ ChainState
initialChainState = Utxo.initial `strictHCons` TipGenesis `strictHCons` HNil

indexBlock
  ∷ ∀ state
   . [ Utxo
     , ChainTip
     , LastIndexedBlock
     , Tagged "submitted" (Set TxId)
     , Tagged "in-ledger" (Set TxId)
     ]
    ∈∈ state
  ⇒ Addresses
  → StdCardanoBlock
  → Finality
  → ChainTip
  → (state → (state, Utxo.IndexingResult))
indexBlock addresses block finality !tip !state = (state', utxoIndexingResult)
 where
  state' ∷ state =
    setter (Tagged @"last-indexed" (SJust (blockRef block))) $
      case utxoIndexingResult of
        Utxo.UtxoUpdateError {} → state
        Utxo.UtxoNotUpdated → state
        Utxo.UtxoUpdated utxo' txs →
          let txIds = Set.fromList (txViewId . transactionViewUtxo <$> txs)
           in setter utxo'
                . setter tip
                . update @(Tagged "in-ledger" (Set TxId)) (Set.union txIds <$>)
                $ state

  utxoIndexingResult ∷ Utxo.IndexingResult =
    Utxo.indexBlock
      (lookTagged @"submitted" @(Set TxId) state)
      addresses
      block
      finality
      (look @Utxo state)

rollbackTo
  ∷ [Utxo, ChainTip, LastIndexedBlock] ∈∈ state
  ⇒ ChainPoint
  → ChainTip
  → state
  → state
rollbackTo point tip chainState =
  chainState
    & setter tip
    & maybe id setter (Utxo.rollbackTo point (look @Utxo chainState))

{- | The security parameter is a non-updatable one:
After how many blocks is the blockchain considered to be final,
and thus can no longer be rolled back
(i.e. what is the maximum allowable length of any chain fork).
-}
securityParam ∷ Word64
securityParam = 2160
