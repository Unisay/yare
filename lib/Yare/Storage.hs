module Yare.Storage
  ( Storage
  , overStorage
  , readOverStorage
  , readStorage
  , inMemory
  , onDisk
  ) where

import Yare.Prelude

import Codec.Serialise (Serialise)
import Data.IORef.Strict qualified as Strict
import Database.LMDB.Simple qualified as LMDB
import Database.LMDB.Simple.Internal qualified as LMDBI
import NoThunks.Class (NoThunks)
import Path (Abs, File, Path, toFilePath)

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
