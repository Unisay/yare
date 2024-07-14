module Yare.Tracer
  ( module Control.Tracer
  , showTracer
  ) where

import Relude

import Control.Tracer
import String.ANSI (blackBg, faint)

showTracer ∷ (Show a, Applicative m) ⇒ String → Tracer m a
showTracer prefix =
  faint . ((blackBg (" " <> prefix <> " ") <> " ") <>) . show >$< debugTracer
