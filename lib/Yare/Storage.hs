module Yare.Storage
  ( Storage (..)
  , ioRefStorage
  ) where

import Relude

data Storage m s = Storage
  { modifyStorage ∷ (s → s) → m ()
  , readState ∷ m s
  }

ioRefStorage ∷ IORef s → Storage IO s
ioRefStorage ref =
  Storage
    { modifyStorage = \f → atomicModifyIORef' ref ((,()) . f)
    , readState = readIORef ref
    }
