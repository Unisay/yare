module Yare.Storage
  ( Storage (..)
  , inMemory
  ) where

import Relude

type Storage ∷ (Type → Type) → Type → Type
data Storage m s = Storage
  { modifyStorage ∷ (s → s) → m ()
  , readState ∷ m s
  }

inMemory ∷ IORef s → Storage IO s
inMemory ref =
  Storage
    { modifyStorage = \f → atomicModifyIORef' ref ((,()) . f)
    , readState = readIORef ref
    }
