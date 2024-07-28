module Data.IORef.Strict
  ( StrictIORef
  , newIORef
  , readIORef
  , writeIORef
  , ThunkException (..)
  , atomicModifyIORef
  ) where

import Relude hiding (readIORef, writeIORef, atomicModifyIORef, newIORef)

import Control.Exception (throw)
import Data.IORef qualified as Lazy
import NoThunks.Class (NoThunks, noThunks, thunkContext)
import NoThunks.Class qualified as NoThunks

type StrictIORef ∷ Type → Type
newtype StrictIORef a = StrictIORef (Lazy.IORef a)

newIORef ∷ (NoThunks a, HasCallStack) ⇒ a → IO (StrictIORef a)
newIORef a = check a >> StrictIORef <$> Lazy.newIORef a

readIORef ∷ StrictIORef a → IO a
readIORef (StrictIORef ref) = Lazy.readIORef ref

writeIORef ∷ (NoThunks a, HasCallStack) ⇒ StrictIORef a → a → IO ()
writeIORef (StrictIORef ref) !x = check x >> Lazy.writeIORef ref x

atomicModifyIORef ∷ StrictIORef a → (a → (a, b)) → IO b
atomicModifyIORef (StrictIORef ref) = Lazy.atomicModifyIORef' ref

check ∷ (NoThunks a, HasCallStack) ⇒ a → IO ()
check x = do
  mThunk ← noThunks [] x
  case mThunk of
    Nothing → pass
    Just thunk → throw $ ThunkException (thunkContext thunk) callStack

type ThunkException ∷ Type
data ThunkException = ThunkException
  { thunkExceptionContext ∷ NoThunks.Context
  , thunkExceptionCallStack ∷ CallStack
  }
  deriving stock (Show)
  deriving anyclass (Exception)
