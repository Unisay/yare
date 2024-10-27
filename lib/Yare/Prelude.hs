module Yare.Prelude
  ( module Reexport
  , (<<&>>)
  , strictHCons
  ) where

import Data.HList as Reexport (HList (HCons, HNil), (.*.))
import Data.Has as Reexport
import Data.Tagged as Reexport (Tagged (Tagged), untag)
import Development.Placeholders as Reexport
import Relude as Reexport hiding (get, state)

(<<&>>) ∷ (Functor f, Functor g) ⇒ f (g a) → (a → b) → f (g b)
(<<&>>) = flip (<<$>>)

infixl 1 <<&>>

-- | Strict version of 'Data.HList.HCons'.
strictHCons ∷ a → HList as → HList (a ': as)
strictHCons !x !xs = HCons x xs

infixr 2 `strictHCons`
