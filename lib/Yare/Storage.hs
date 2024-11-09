module Yare.Storage
  ( StorageMgr (..)
  , storageManager
  , readDefaultStorage
  , overDefaultStorage
  , Storage
  , overStorage
  , readOverStorage
  , readStorage
  , inMemory
  , onDisk
  ) where

import Yare.Prelude

import Codec.Serialise (Serialise)
import Control.Tracer.Extended (Tracer, traceWith)
import Data.IORef.Strict qualified as Strict
import Database.LMDB.Simple qualified as LMDB
import Database.LMDB.Simple.Internal qualified as LMDBI
import NoThunks.Class (NoThunks)
import Path (Abs, File, Path, toFilePath)
import Yare.App.Types (StorageMode (..))

data StorageMgr m s = StorageMgr
  { volatileStorage ∷ Storage m s
  , durableStorage ∷ Storage m s
  , defaultStorage ∷ m (Storage m s)
  , storageMode ∷ m StorageMode
  , setStorageMode ∷ StorageMode → m ()
  }

storageManager
  ∷ ∀ s
   . NFData s
  ⇒ StorageMode
  → Tracer IO StorageMode
  → Tagged "volatile" (Storage IO s)
  → Tagged "durable" (Storage IO s)
  → IO (StorageMgr IO s)
storageManager mode modeTracer volatile durable = do
  modeRef ← newIORef mode
  let storageMode = readIORef modeRef
  let volatileStorage = untag volatile
  let durableStorage = untag durable
  let defaultStorage =
        storageMode <&> \case
          Volatile → volatileStorage
          Durable → durableStorage
  let setStorageMode newMode = do
        oldMode ← storageMode
        unless (oldMode == newMode) do
          case newMode of
            -- Current mode switching is not atomic:
            -- if some other thread is modifying the storage
            -- while we are switching the mode, the updates
            -- could end up in an inconsistent state.
            Volatile →
              -- Copy the durable storage to the volatile storage
              readStorage durableStorage
                -- Force the storage state to avoid thunks
                >>= writeStorage volatileStorage . force
            Durable →
              -- Copy the volatile storage to the durable storage
              readStorage volatileStorage >>= writeStorage durableStorage
          -- Switch to the new storage
          writeIORef modeRef newMode
          traceWith modeTracer newMode
  pure StorageMgr {..}

readDefaultStorage ∷ ∀ s env m. Monad m ⇒ StorageMgr m s ∈ env ⇒ env → m s
readDefaultStorage = readStorage <=< defaultStorage . look

overDefaultStorage
  ∷ ∀ s t a b m
   . (Monad m, StorageMgr m s ∈ t)
  ⇒ t
  → (s → (s, a))
  → (a → m b)
  → m b
overDefaultStorage env f after = do
  storage ← defaultStorage (look env)
  overStorage storage f after

data Storage (m ∷ Type → Type) (s ∷ Type) = Storage
  { overStorage ∷ ∀ a b. (s → (s, a)) → (a → m b) → m b
  -- ^ The workhorse of the storage that acts as a bridge
  -- between a pure core and impure imperative shell (runtime):
  -- takes a pure function that modifies the state
  -- yielding a new state and a result,
  -- returns a potentially impure computation 'a` yielding
  -- the result and updating the state.
  , readStorage ∷ m s
  -- ^ Could be implemented in terms of the 'overStorage' above
  -- using the 'readOverStorage' helper.
  }

-- | Helper function to implement 'readStorage' in terms of 'overStorage'.
readOverStorage ∷ (∀ a. (s → (s, a)) → m a) → m s
readOverStorage overStorage = overStorage \s → (s, s)

-- | Replace the storage state with a new one.
writeStorage ∷ Applicative m ⇒ Storage m s → s → m ()
writeStorage storage s = overStorage storage (const (s, ())) pure

-- | A simple in-memory storage.
inMemory ∷ (HasCallStack, NoThunks s) ⇒ s → IO (Storage IO s)
inMemory !s0 = do
  ref ← Strict.newIORef s0
  pure
    Storage
      { overStorage = \f after → do
          s ← Strict.readIORef ref
          let (s', a) = f s
          after a >>= (Strict.writeIORef ref s' $>)
      , readStorage = Strict.readIORef ref
      }

onDisk ∷ ∀ s. Serialise s ⇒ Path Abs File → s → IO (Storage IO s)
onDisk (toFilePath → fp) !s0 = do
  lmdb ← LMDB.openReadWriteEnvironment fp LMDB.defaultLimits
  pure
    Storage
      { overStorage = \pureStateTransition after →
          LMDB.readWriteTransaction lmdb do
            s ∷ s ← readStoredData
            let (s', a) = pureStateTransition s
            b ← liftIO (after a)
            writeStoredData s'
            pure b
      , readStorage = LMDB.readOnlyTransaction lmdb readStoredData
      }
 where
  readStoredData ∷ LMDBI.IsMode m ⇒ LMDB.Transaction m s
  readStoredData = do
    db ∷ LMDB.Database Word s ← LMDB.getDatabase databaseName
    LMDB.get db databaseSlot >>= \case
      Nothing → pure s0 -- Should only happen first time
      Just as → pure as

  writeStoredData ∷ s → LMDBI.Transaction LMDBI.ReadWrite ()
  writeStoredData s = do
    db ∷ LMDB.Database Word s ← LMDB.getDatabase databaseName
    LMDB.put db databaseSlot (Just s)

  databaseName ∷ Maybe String = Nothing -- "Anonymous" database has no name
  databaseSlot ∷ Word = 0 -- The only key in the database
