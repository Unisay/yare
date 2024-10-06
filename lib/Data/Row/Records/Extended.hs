module Data.Row.Records.Extended
  ( module Reexport
  , set
  , over
  ) where

import Data.Row.Records as Reexport

over ∷ (KnownSymbol l, HasType l a r) ⇒ Label l → (a → a) → Rec r → Rec r
over l f r = set l (f (r .! l)) r

set ∷ ∀ l r a. (KnownSymbol l, HasType l a r) ⇒ Label l → a → Rec r → Rec r
set = update
