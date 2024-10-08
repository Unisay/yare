module Yare.Storage
  ( Storage
  , overStorage
  , readOverStorage
  , readStorage
  , readStorageField
  , inMemory
  ) where

import Yare.Prelude

import Data.IORef.Strict (StrictIORef)
import Data.IORef.Strict qualified as Strict
import NoThunks.Class (NoThunks)

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

-- | Reads a field value from the storage state.
readStorageField
  ∷ ∀ s l a m
   . (KnownSymbol l, Functor m, HasType l a s)
  ⇒ Storage m (Rec s)
  → Label l
  → m a
readStorageField storage label = do
  s ← readStorage storage
  pure $ s .! label

-- | A simple in-memory storage.
inMemory ∷ NoThunks s ⇒ StrictIORef s → Storage IO s
inMemory ref =
  Storage
    { overStorage = \f after → do
        s ← Strict.readIORef ref
        let (s', a) = f s
        after a >>= (Strict.writeIORef ref s' $>)
    , readStorage = Strict.readIORef ref
    }
