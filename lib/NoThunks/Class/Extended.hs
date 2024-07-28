module NoThunks.Class.Extended
  ( module Reexport
  , repeatedly
  ) where

import Relude

import NoThunks.Class (NoThunks (..), ThunkInfo (thunkContext), unsafeNoThunks)
import NoThunks.Class qualified as Reexport
import NoThunks.Class.Orphans qualified as Reexport ()

repeatedly ∷ ∀ a b. (NoThunks b, HasCallStack) ⇒ (b → a → b) → b → [a] → b
repeatedly f = go
 where
  go ∷ b → [a] → b
  go !b = \case
    [] → b
    a : as →
      let !b' = f b a
       in case unsafeNoThunks b' of
            Nothing → go b' as
            Just thunk →
              error
                ( "Unexpected thunk with context "
                    <> show (thunkContext thunk)
                )
