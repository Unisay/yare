module Yare.Storage
  ( Storage
  , overStorage
  , overStorageState
  , readOverStorage
  , readStorage
  , zoomStorage
  , inMemory
  , stateful
  , stateful'
  ) where

import Relude

import Control.Lens (Lens, Simple, over, set, view)

type Storage ∷ (Type → Type) → Type → Type
data Storage m s = Storage
  { overStorage ∷ ∀ a. (s → (s, a)) → m a
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

-- | Given a storage and a pure stateful computation,
-- | produces an impure computation.
overStorageState ∷ ∀ s m a. Storage m s → State s a → m a
overStorageState storage st = overStorage storage (swap . runState st)

-- | A simple in-memory storage.
inMemory ∷ IORef s → Storage IO s
inMemory ref =
  Storage
    { overStorage = atomicModifyIORef' ref
    , readStorage = readIORef ref
    }

-- | Uses a lens to produce another storage
-- | focused on a part of the original storage state.
zoomStorage ∷ Functor m ⇒ Simple Lens t s → Storage m t → Storage m s
zoomStorage lens Storage {overStorage, readStorage} =
  Storage
    { overStorage = \f → overStorage \t →
        let (t', a) = f (view lens t)
         in (set lens t' t, a)
    , readStorage =
        view lens <$> readStorage
    }

-- | A helper to lift pure state transition
stateful ∷ (s → (s, a)) → State s a
stateful f = state (swap . f)

-- | A helper to lift pure sub-state transition using a lens
stateful' ∷ Simple Lens s t → (t → (t, a)) → State s a
stateful' l f = state \s →
  let (t, a) = f (view l s)
   in (a, set l t s)
