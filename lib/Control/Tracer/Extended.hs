module Control.Tracer.Extended
  ( module Control.Tracer
  , prettyTracer
  , lineTracer
  , withPrefix
  , withFaint
  , showTracer
  ) where

import Yare.Prelude

import Control.Tracer
import Fmt (Buildable, pretty)
import String.ANSI (faint, underline)

prettyTracer ∷ (Buildable a, Applicative m) ⇒ Tracer m a
prettyTracer = pretty >$< debugTracer

withFaint ∷ Tracer m String → Tracer m String
withFaint tr = faint >$< tr

withPrefix ∷ String → Tracer m String → Tracer m String
withPrefix prefix tr =
  ((underline (toString prefix) <> " ") <>) >$< tr

{- | A tracer that prints to stdout a single line of text
overwriting the previous one.
-}
lineTracer ∷ Tracer IO String
lineTracer = Tracer \s → do
  putStr "\r\ESC[K" -- clears line
  putStrLn s -- prints the message
  hFlush stdout -- flushes the buffer
  putStr "\ESC[A" -- move cursor one line up
  putStr "\r\ESC[K" -- clears line

showTracer :: Show showable => Tracer IO showable
showTracer = show >$< debugTracer
