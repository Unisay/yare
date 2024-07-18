{- |
Module: Main
Description: Reads command line arguments and starts the Yare application.
-}
module Main (main) where

import Relude

import Data.Tagged (Tagged (..))
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
import Options.Applicative.Help.Pretty (vsep)
import Ouroboros.Network.Magic (NetworkMagic (NetworkMagic))
import Path (File, SomeBase (..), parseSomeFile)
import Path.IO qualified as Path
import Yare.App qualified as Yare
import Yare.App.Types qualified as App
import Yare.Chain.Point (ChainPoint, parseChainPoint)
import Yare.Config.Toml qualified as Toml
import Yare.Node.Socket (NodeSocket (..))

main ∷ IO ()
main = withUtf8 do
  Args {networkMagic, nodeSocketPath, mnemonicPath, syncFrom} ← parseArguments
  Toml.Config {apiHttpPort, txId} ← Toml.readConfig
  nodeSocket ←
    NodeSocket <$> case nodeSocketPath of
      Path.Abs a → pure a
      Path.Rel r → Path.makeAbsolute r
  mnemonicFile ←
    Tagged @"mnemonic" <$> case mnemonicPath of
      Path.Abs a → pure a
      Path.Rel r → Path.makeAbsolute r
  Yare.start
    App.Config
      { apiHttpPort
      , nodeSocket
      , networkMagic
      , mnemonicFile
      , syncFrom
      , txId
      }

type Args ∷ Type
data Args = Args
  { nodeSocketPath ∷ SomeBase File
  , networkMagic ∷ NetworkMagic
  , mnemonicPath ∷ SomeBase File
  , syncFrom ∷ Maybe ChainPoint
  }

parseArguments ∷ IO Args
parseArguments =
  execParser $
    info
      (options <**> helper)
      (fullDesc <> progDesc "Yare")

options ∷ Parser Args
options =
  Args
    <$> nodeSocketPathOption
    <*> networkMagicOption
    <*> mnemonicFileOption
    <*> optional syncFromChainPoint
 where
  nodeSocketPathOption ∷ Parser (SomeBase File) =
    option
      (eitherReader (first displayException . parseSomeFile))
      ( fold
          [ metavar "CARDANO_NODE_SOCKET_PATH"
          , long "node-socket"
          , helpDoc $ Just "Path to the Cardano Node socket file."
          ]
      )
  networkMagicOption ∷ Parser NetworkMagic =
    option
      (eitherReader (bimap toString NetworkMagic . readEither))
      ( fold
          [ metavar "NETWORK_MAGIC"
          , long "network-magic"
          , helpDoc . Just $
              "A magic number used to discriminate \
              \between different Cardano networks. "
          ]
      )
  mnemonicFileOption ∷ Parser (SomeBase File) =
    option
      (eitherReader (first displayException . parseSomeFile))
      ( fold
          [ metavar "MNEMONIC_FILE"
          , long "mnemonic-file"
          , helpDoc $
              Just
                "Path to a file with mnemonic phrase \
                \(space separated list of 24 BIP-39 dictionary words)."
          ]
      )

  syncFromChainPoint ∷ Parser ChainPoint =
    option
      (eitherReader parseChainPoint)
      ( fold
          [ metavar "BLOCK_HASH:SLOT_NO"
          , long "sync-from-chain-point"
          , helpDoc . Just . vsep $
              [ "Sync from the given chain point."
              , "Example: 'b0b33e2980f01dcee60c8884ee46a3a601b945055eadd1f01b\
                \a1c24c8f9e7fc5:41683132'"
              ]
          ]
      )
