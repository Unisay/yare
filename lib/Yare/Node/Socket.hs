module Yare.Node.Socket
  ( NodeSocket (..)
  , nodeSnocketPath
  , nodeSnocketFilePath
  , nodeSocketLocalAddress
  ) where

import Relude

import Ouroboros.Network.Snocket (LocalAddress, localAddressFromPath)
import Path (Abs, File, Path, toFilePath)

newtype NodeSocket = NodeSocket (Path Abs File)

nodeSnocketPath ∷ NodeSocket → Path Abs File
nodeSnocketPath (NodeSocket path) = path

nodeSnocketFilePath ∷ NodeSocket → FilePath
nodeSnocketFilePath = toFilePath . nodeSnocketPath

nodeSocketLocalAddress ∷ NodeSocket → LocalAddress
nodeSocketLocalAddress = localAddressFromPath . nodeSnocketFilePath
