module Data.HList.Extended
  ( module Reexport
  , strictHCons
  ) where

import Data.HList.HList as Reexport

{-# INLINE strictHCons #-}
strictHCons ∷ h → HList t → HList (h : t)
strictHCons !x !xs = HCons x xs

infixr 5 `strictHCons`
