-- | Utility functions for working with stateful computations.
module Yare.Util.State
  ( stateField
  , stateMay
  ) where

import Control.Monad.State.Class (MonadState (state))
import Data.Maybe (Maybe (..))
import Data.Row.Records (KnownSymbol, Label, Rec, (.!), type (.!), type (≈))
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
  ∷ ∀ m s row a label
   . ( row .! label ≈ s
     , KnownSymbol label
     , MonadState (Rec row) m
     )
  ⇒ Label label
  -- ^ The label of the record field to update.
  → (s → (s, a))
  -- ^ The state transition function to apply to the field value.
  → m a
  -- ^ Resulting MonadState-ful computation
stateField fieldLabel f = state \oritinalRecord →
  let (newFieldValue, a) = f (oritinalRecord .! fieldLabel)
   in (a, Rec.update fieldLabel newFieldValue oritinalRecord)

{- | Lift an optional (which may not be possible) state transition
to a stateful computation.
-}
stateMay ∷ MonadState s m ⇒ (s → Maybe (s, a)) → m (Maybe a)
stateMay f = state \originalState →
  case f originalState of
    Nothing → (Nothing, originalState)
    Just (updatedState, a) → (Just a, updatedState)
