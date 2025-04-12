-- | Utility functions for working with stateful computations.
module Yare.Util.State
  ( usingMonadState
  , liftStF
  , stateHas
  ) where

import Yare.Prelude

import Control.Monad.State.Class (MonadState (state))

-- A partial state transition function.
type StF s a = s → Maybe (s, a)

{- | Lift an optional (which may not be possible) state transition
to a stateful computation.
-}
usingMonadState ∷ ∀ m a s t. (MonadState s m, t ∈ s) ⇒ StF t a → m (Maybe a)
usingMonadState f = state \originalState →
  case liftStF f originalState of
    Nothing → (Nothing, originalState)
    Just (updatedState, a) → (Just a, updatedState)

{- | Given a partial state transition function that operates on a part of the
state, lift it to a partial state transition function that operates
on the whole state.
-}
liftStF ∷ ∀ a t s. t ∈ s ⇒ StF t a → StF s a
liftStF f s = first (`setter` s) <$> f (look @t s)

stateHas ∷ ∀ a s m b. (MonadState s m, a ∈ s) ⇒ (a → (a, b)) → m b
stateHas f = state \originalState →
  let (a, b) = f (look @a originalState)
   in (b, setter a originalState)
