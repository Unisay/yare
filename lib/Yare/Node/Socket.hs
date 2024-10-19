module Yare.Node.Socket
  ( NodeSocket (..)
  , nodeSocketLocalAddress
  ) where

import Ouroboros.Network.Snocket (LocalAddress, localAddressFromPath)
import Path (Abs, File, Path, toFilePath)

newtype NodeSocket = NodeSocket (Path Abs File)

nodeSocketLocalAddress ∷ NodeSocket → LocalAddress
nodeSocketLocalAddress (NodeSocket path) =
  localAddressFromPath (toFilePath path)
