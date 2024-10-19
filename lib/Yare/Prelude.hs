module Yare.Prelude
  ( module Reexport
  , (<<&>>)
  ) where

import Data.HList as Reexport (HList (HCons, HNil), (.*.))
import Data.Has as Reexport
import Data.Tagged as Reexport (Tagged (Tagged), untag)
import Development.Placeholders as Reexport
import Relude as Reexport hiding (get, state)

(<<&>>) ∷ (Functor f, Functor g) ⇒ f (g a) → (a → b) → f (g b)
(<<&>>) = flip (<<$>>)

infixl 1 <<&>>
