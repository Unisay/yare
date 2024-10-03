module Yare.Tracer
  ( module Control.Tracer
  , prettyTracer
  , withPrefix
  ) where

import Yare.Prelude

import Control.Tracer
import Fmt (Buildable, pretty)
import String.ANSI (blackBg, faint)

prettyTracer ∷ (Buildable a, Applicative m) ⇒ Tracer m a
prettyTracer = pretty >$< debugTracer

withPrefix ∷ String → Tracer m String → Tracer m String
withPrefix prefix tr =
  faint . ((blackBg (" " <> toString prefix <> " ") <> " ") <>) >$< tr
