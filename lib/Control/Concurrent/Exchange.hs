module Control.Concurrent.Exchange
  ( Exchange
  , newExchange

    -- * Client's interface
  , writeInput
  , readOutput
  , exchangeInput

    -- * Server's interface
  , readInput
  , writeOutput
  , processInput
  ) where

import Control.Concurrent.Class.MonadMVar (MonadMVar (..))

import Yare.Prelude hiding (MVar, newEmptyMVar, newMVar, putMVar, takeMVar)

type Exchange ∷ (Type → Type) → Type → Type → Type
data Exchange m i o = Exchange (MVar m i) (MVar m o)

newExchange ∷ MonadMVar m ⇒ m (Exchange m i o)
newExchange = Exchange <$> newEmptyMVar <*> newEmptyMVar

writeInput ∷ MonadMVar m ⇒ Exchange m i o → i → m ()
writeInput (Exchange inVar _outVar) = putMVar inVar

readOutput ∷ MonadMVar m ⇒ Exchange m i o → m o
readOutput (Exchange _inVar outVar) = takeMVar outVar

readInput ∷ MonadMVar m ⇒ Exchange m i o → m i
readInput (Exchange inVar _) = takeMVar inVar

writeOutput ∷ MonadMVar m ⇒ Exchange m i o → o → m ()
writeOutput (Exchange _inVar outVar) = putMVar outVar

processInput ∷ MonadMVar m ⇒ Exchange m i o → (i → m o) → m ()
processInput ex f = do
  i ← readInput ex
  o ← f i
  writeOutput ex o

exchangeInput ∷ MonadMVar m ⇒ Exchange m i o → i → m o
exchangeInput ex i = do
  writeInput ex i
  readOutput ex
