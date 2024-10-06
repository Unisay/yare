module Yare.Prelude
  ( module Reexport
  , Open
  , (<<&>>)
  ) where

import Data.Row.Internal (LT ((:->)), Row (R), Unconstrained)
import Data.Row.Records as Reexport hiding
  ( Map
  , empty
  , map
  , sequence
  , traverse
  , zip
  )
import Relude as Reexport hiding (get, state)

type family Open (r ∷ Row Type) (rest ∷ Row Type) ∷ Constraint where
  Open (R '[]) _ = Unconstrained
  Open (R (l :-> a ': t)) rest = (HasType l a rest, Open (R t) rest)

(<<&>>) ∷ (Functor f, Functor g) ⇒ f (g a) → (a → b) → f (g b)
(<<&>>) = flip (<<$>>)

infixl 1 <<&>>
