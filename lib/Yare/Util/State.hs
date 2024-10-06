-- | Utility functions for working with stateful computations.
module Yare.Util.State
  ( stateField
  , setStateField
  , overStateField
  , stateMay
  ) where

import Yare.Prelude

import Control.Monad.State.Class (MonadState (state))
import Data.Row.Records qualified as Rec

{- | Provides a way to interact with a record field in a stateful manner.

Takes a label identifying a field within a record, and a function that
operates on the current value of that field, producing a new value for the
field and an output value.

Updates the record with the new field value and returns
the output value. This is all done within a 'MonadState' context, allowing
for stateful computations.

Example:

@
data MyRecord = MyRecord { field1 :: Int, field2 :: String }

myFunction :: MonadState (Rec MyRecord) m => m String
myFunction = stateField #field1 (\x -> (x + 1, show x))
@

In this example, 'myFunction' uses 'stateField' to access the 'field1'
of a 'MyRecord'. The lambda function takes the current value of 'field1',
increments it, and returns the old value as a string. 'myFunction' then
updates the record with the new 'field1' value and returns the string.
-}
stateField
  ∷ ∀ m r l a b state
   . ( HasType l a r
     , KnownSymbol l
     , MonadState state m
     , state ≈ Rec r
     )
  ⇒ Label l
  -- ^ The label of the record field to update.
  → (a → (a, b))
  -- ^ The state transition function to apply to the field value.
  → m b
  -- ^ Resulting MonadState-ful computation
stateField fieldLabel f = state \oritinalRecord →
  let (!newFieldValue, !b) = f (oritinalRecord .! fieldLabel)
   in (b, Rec.update fieldLabel newFieldValue oritinalRecord)

{- | Lift a field modification function to
a stateful computation acting on the whole state.
-}
overStateField
  ∷ ∀ m s r l
   . ( HasType l s r
     , KnownSymbol l
     , MonadState (Rec r) m
     )
  ⇒ Label l
  -- ^ The label of the record field to update.
  → (s → s)
  -- ^ The state transition function to apply to the field value.
  → m ()
  -- ^ Resulting MonadState-ful computation
overStateField fieldLabel f =
  stateField fieldLabel \x → let !a = f x in (a, ())

setStateField
  ∷ ∀ m s r l
   . ( HasType l s r
     , KnownSymbol l
     , MonadState (Rec r) m
     )
  ⇒ Label l
  -- ^ The label of the record field to update.
  → s
  -- ^ The new value of the field.
  → m ()
  -- ^ Resulting MonadState-ful computation
setStateField fieldLabel !s =
  stateField fieldLabel \_oldValue → (s, ())

{- | Lift an optional (which may not be possible) state transition
to a stateful computation.
-}
stateMay ∷ MonadState s m ⇒ (s → Maybe (s, a)) → m (Maybe a)
stateMay f = state \originalState →
  case f originalState of
    Nothing → (Nothing, originalState)
    Just (updatedState, a) → (Just a, updatedState)
