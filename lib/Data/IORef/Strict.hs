module Data.IORef.Strict
  ( StrictIORef
  , newIORef
  , readIORef
  , writeIORef
  , atomicModifyIORef
  ) where

import Yare.Prelude hiding (atomicModifyIORef, newIORef, readIORef, writeIORef)

import Data.IORef qualified as Lazy
import NoThunks.Class.Extended (NoThunks, checkThunksIO)

type StrictIORef ∷ Type → Type
newtype StrictIORef a = StrictIORef (Lazy.IORef a)

newIORef ∷ (NoThunks a, HasCallStack) ⇒ a → IO (StrictIORef a)
newIORef a = checkThunksIO a >> StrictIORef <$> Lazy.newIORef a

readIORef ∷ StrictIORef a → IO a
readIORef (StrictIORef ref) = Lazy.readIORef ref

writeIORef ∷ (NoThunks a, HasCallStack) ⇒ StrictIORef a → a → IO ()
writeIORef (StrictIORef ref) !x = checkThunksIO x >> Lazy.writeIORef ref x

atomicModifyIORef ∷ StrictIORef a → (a → (a, b)) → IO b
atomicModifyIORef (StrictIORef ref) = Lazy.atomicModifyIORef' ref
