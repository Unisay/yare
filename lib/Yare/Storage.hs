module Yare.Storage
  ( Storage
  , overStorage
  , overStorageState
  , readStorage
  , zoomStorage
  , inMemory
  , stateful
  , stateful'
  ) where

import Relude

import Control.Lens (Lens, Simple, set, view)

type Storage ∷ (Type → Type) → Type → Type
data Storage m s = Storage
  { overStorage ∷ ∀ a. (s → (s, a)) → m a
  , readStorage ∷ m s
  }

overStorageState ∷ ∀ s m a. Storage m s → State s a → m a
overStorageState storage st = overStorage storage (swap . runState st)

inMemory ∷ IORef s → Storage IO s
inMemory ref =
  Storage
    { overStorage = atomicModifyIORef' ref
    , readStorage = readIORef ref
    }

zoomStorage ∷ Functor m ⇒ Simple Lens t s → Storage m t → Storage m s
zoomStorage lens Storage {..} =
  Storage
    { overStorage = \f → overStorage \t →
        let (t', a) = f (view lens t)
         in (set lens t' t, a)
    , readStorage =
        view lens <$> readStorage
    }

stateful ∷ (s → (s, a)) → State s a
stateful f = state (swap . f)

stateful' ∷ Simple Lens s t → (t → (t, a)) → State s a
stateful' l f = state \s →
  let (t, a) = f (view l s)
   in (a, set l t s)
