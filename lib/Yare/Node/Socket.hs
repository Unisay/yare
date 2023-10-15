module Yare.Node.Socket
  ( NodeSocket (..)
  , nodeSocketLocalAddress
  ) where

import Ouroboros.Network.Snocket (LocalAddress, localAddressFromPath)
import Path

newtype NodeSocket = NodeSocket (Path Abs File)

nodeSocketLocalAddress ∷ NodeSocket → LocalAddress
nodeSocketLocalAddress (NodeSocket path) =
  localAddressFromPath (toFilePath path)
