{- |
Module: Main
Description: Reads command line arguments and starts the Yare application.
-}
module Main (main) where

import Yare.Prelude

import Control.Exception (catch)
import Data.Maybe.Strict (maybeToStrictMaybe)
import Data.String qualified as String
import GHC.Exception (prettyCallStackLines)
import Main.Utf8 (withUtf8)
import NoThunks.Class.Extended
  ( ThunkException
  , thunkExceptionCallStack
  , thunkExceptionContext
  )
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
import Options.Applicative.Builder (value)
import Options.Applicative.Help.Pretty (vsep)
import Ouroboros.Network.Magic (NetworkMagic (NetworkMagic))
import Path (File, SomeBase (..), parseSomeFile)
import Path.IO qualified as Path
import Yare.App qualified as Yare
import Yare.App.Types (StorageMode (..))
import Yare.Chain.Point (parseChainPoint)
import Yare.Chain.Types (ChainPoint, SyncFrom)
import Yare.Node.Socket (NodeSocket (..))

main ∷ IO ()
main = withUtf8 do
  args ← parseArguments
  nodeSocket ←
    NodeSocket
      <$> case argNodeSocketPath args of
        Path.Abs a → pure a
        Path.Rel r → Path.makeAbsolute r
  mnemonicFile ←
    Tagged @"mnemonic"
      <$> case argMnemonicPath args of
        Path.Abs a → pure a
        Path.Rel r → Path.makeAbsolute r
  storageMode ←
    case argStorageMode args of
      InMemory → pure InMemory
      OnDisk databasePath →
        OnDisk
          <$> case databasePath of
            Path.Abs a → pure (Tagged a)
            Path.Rel r → Tagged <$> Path.makeAbsolute r
  Yare.start
    ( 9999
        `strictHCons` nodeSocket
        `strictHCons` argNetworkMagic args
        `strictHCons` argSyncFrom args
        `strictHCons` mnemonicFile
        `strictHCons` storageMode
        `strictHCons` HNil
    )
    `catch` \(te ∷ ThunkException) → do
      putStrLn "The application encountered an unexpected thunk:"
      case thunkExceptionContext te of
        Left ctx →
          putStrLn $ "Context:\n" <> String.unlines (("- " ++) <$> reverse ctx)
        Right thunkInfo → putStrLn $ "Info: " ++ show thunkInfo
      putStrLn . String.unlines . prettyCallStackLines $
        thunkExceptionCallStack te

data Args = Args
  { argNodeSocketPath ∷ SomeBase File
  , argNetworkMagic ∷ NetworkMagic
  , argMnemonicPath ∷ SomeBase File
  , argSyncFrom ∷ SyncFrom
  , argStorageMode ∷ StorageMode (SomeBase File)
  }

parseArguments ∷ IO Args
parseArguments =
  execParser $ info (options <**> helper) (fullDesc <> progDesc "Yare")

options ∷ Parser Args
options =
  Args
    <$> nodeSocketPathOption
    <*> networkMagicOption
    <*> mnemonicFileOption
    <*> (fmap maybeToStrictMaybe . Tagged <$> optional syncFromChainPoint)
    <*> storageModeOption
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

  storageModeOption ∷ Parser (StorageMode (SomeBase File)) =
    option
      (eitherReader (fmap OnDisk . first displayException . parseSomeFile))
      ( fold
          [ metavar "DATABASE_FILE"
          , long "database-file"
          , value InMemory
          , helpDoc . Just $
              "Path to a file where the application will store its state.\
              \If omitted, the application will use in-memory storage."
          ]
      )
