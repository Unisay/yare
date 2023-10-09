module Main (main) where

import Relude

import Main.Utf8 (withUtf8)
import Options.Applicative
  ( Parser
  , eitherReader
  , execParser
  , fullDesc
  , helpDoc
  , helper
  , info
  , long
  , metavar
  , option
  , progDesc
  )
import Path (File, SomeBase (..), parseSomeFile)
import Path.IO qualified as Path
import Yare.Client qualified as Yare
import Yare.Data.Node.Socket (NodeSocket (..))

main ∷ IO ()
main = withUtf8 do
  Args {nodeSocketPath} ← parseArguments
  nodeSocket ←
    NodeSocket <$> case nodeSocketPath of
      Path.Abs a → pure a
      Path.Rel r → Path.makeAbsolute r
  void $ Yare.main nodeSocket

newtype Args = Args {nodeSocketPath ∷ SomeBase File}

parseArguments ∷ IO Args
parseArguments =
  execParser $
    info
      (options <**> helper)
      (fullDesc <> progDesc "Yare")

options ∷ Parser Args
options = Args <$> nodeSocketPathOption
 where
  nodeSocketPathOption =
    option
      (eitherReader (first displayException . parseSomeFile))
      ( fold
          [ metavar "CARDANO_NODE_SOCKET_PATH"
          , long "node-socket"
          , helpDoc . Just $ "Path to the Cardano Node socket file."
          ]
      )
