module NoThunks.Class.Extended
  ( module Reexport
  , checkThunksIO
  , unsafeCheckThunks
  , foldlNoThunks
  , ThunkException (..)
  ) where

import Yare.Prelude

import Control.Exception (throwIO)
import NoThunks.Class
  ( Context
  , Info
  , NoThunks (..)
  , ThunkInfo (..)
  , unsafeNoThunks
  )
import NoThunks.Class qualified as Reexport
import NoThunks.Class.Orphans qualified as Reexport ()

checkThunksIO ∷ (NoThunks a, HasCallStack) ⇒ a → IO ()
checkThunksIO x =
  noThunks [] x >>= \case
    Nothing → pass
    Just (ThunkInfo contextOrInfo) →
      throwIO $ ThunkException contextOrInfo callStack

unsafeCheckThunks ∷ (NoThunks a, HasCallStack) ⇒ a → a
unsafeCheckThunks x =
  case unsafeNoThunks x of
    Nothing → x
    Just thunk → error ("Unexpected thunk with context " <> show thunk)

-- | Left fold over a list, checking for thunks at each step.
foldlNoThunks ∷ ∀ a b. (NoThunks b, HasCallStack) ⇒ (b → a → b) → b → [a] → b
foldlNoThunks f = go
 where
  go ∷ b → [a] → b
  go !b = \case
    [] → b
    a : as →
      let !b' = f b a
       in case unsafeNoThunks b' of
            Nothing → go b' as
            Just thunk → error ("Unexpected thunk with context " <> show thunk)

data ThunkException = ThunkException
  { thunkExceptionContext ∷ Either Context Info
  , thunkExceptionCallStack ∷ CallStack
  }
  deriving stock (Show)
  deriving anyclass (Exception)
