module Yare.Tracer
  ( module Control.Tracer
  , prefixTracer
  , prefixTracerShow
  ) where

import Yare.Prelude

import Control.Tracer
import String.ANSI (blackBg, faint)

prefixTracerShow ∷ (Show a, Applicative m) ⇒ String → Tracer m a
prefixTracerShow prefix = show >$< prefixTracer prefix

prefixTracer ∷ Applicative m ⇒ String → Tracer m Text
prefixTracer prefix =
  faint
    . ((blackBg (" " <> prefix <> " ") <> " ") <>)
    . toString
    >$< debugTracer
