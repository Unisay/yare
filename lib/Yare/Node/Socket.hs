module Yare.Node.Socket
  ( NodeSocket (..)
  , nodeSocketLocalAddress
  ) where

import Data.Kind (Type)
import Ouroboros.Network.Snocket (LocalAddress, localAddressFromPath)
import Path

type NodeSocket ∷ Type
newtype NodeSocket = NodeSocket (Path Abs File)

nodeSocketLocalAddress ∷ NodeSocket → LocalAddress
nodeSocketLocalAddress (NodeSocket path) =
  localAddressFromPath (toFilePath path)
