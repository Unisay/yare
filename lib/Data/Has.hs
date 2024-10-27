module Data.Has where

import Relude

import Data.HList.Extended (HList (..), hHead, strictHCons)
import Data.Tagged (Tagged, untag)
import GHC.TypeError (ErrorMessage (..), TypeError)
import Relude.Extra.Lens (Lens')

class a ∈ t where
  {-# MINIMAL look, update | hasLens #-}
  look ∷ t → a
  look = getConst . hasLens Const

  update ∷ (a → a) → t → t
  update f t = runIdentity (hasLens (Identity . f) t)

  hasLens ∷ Lens' t a
  hasLens afa t = (\a → update (const a) t) <$> afa (look t)

{-# INLINE setter #-}
setter ∷ ∀ a t. a ∈ t ⇒ a → t → t
setter = update . const

lookTagged ∷ ∀ l a t. Tagged l a ∈ t ⇒ t → a
lookTagged = untag @l . look @(Tagged l a)

instance a ∈ a where
  {-# INLINEABLE look #-}
  look = id
  {-# INLINEABLE update #-}
  update = id

instance {-# OVERLAPPING #-} a ∈ (a, tail) where
  {-# INLINEABLE look #-}
  look = fst
  {-# INLINEABLE update #-}
  update f (a, b) = (f a, b)

instance {-# OVERLAPPABLE #-} a ∈ tail ⇒ a ∈ (head, tail) where
  {-# INLINEABLE look #-}
  look (_, x) = look x
  {-# INLINEABLE update #-}
  update f (a, b) = (a, update f b)

instance
  TypeError
    ( 'Text "Constraint `"
        :<>: 'ShowType a
        :<>: 'Text " ∈ ...` is not satisfied."
    )
  ⇒ a ∈ HList '[]
  where
  look = error "unreachable"
  update = error "unreachable"

instance {-# OVERLAPPING #-} a ∈ HList (a : ts) where
  {-# INLINEABLE look #-}
  look = hHead
  update f (HCons a ts) = strictHCons (f a) ts

instance {-# OVERLAPPABLE #-} b ∈ HList ts ⇒ b ∈ HList (a : ts) where
  {-# INLINEABLE look #-}
  look (HCons _ ts) = look ts
  {-# INLINEABLE update #-}
  update f (HCons a ts) = strictHCons a (update f ts)

type family (∈∈) (ts ∷ [Type]) (r ∷ Type) ∷ Constraint where
  '[] ∈∈ _ = ()
  (t ': ts) ∈∈ r = (t ∈ r, ts ∈∈ r)
