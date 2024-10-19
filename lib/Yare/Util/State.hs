-- | Utility functions for working with stateful computations.
module Yare.Util.State
  ( stateMay
  , stateHas
  ) where

import Yare.Prelude

import Control.Monad.State.Class (MonadState (state))

{- | Lift an optional (which may not be possible) state transition
to a stateful computation.
-}
stateMay ∷ MonadState s m ⇒ (s → Maybe (s, a)) → m (Maybe a)
stateMay f = state \originalState →
  case f originalState of
    Nothing → (Nothing, originalState)
    Just (updatedState, a) → (Just a, updatedState)

stateHas ∷ ∀ a s m b. (MonadState s m, a ∈ s) ⇒ (a → (a, b)) → m b
stateHas f = state \originalState →
  let (a, b) = f (look @a originalState)
   in (b, setter a originalState)
