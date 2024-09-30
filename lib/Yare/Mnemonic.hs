module Yare.Mnemonic
  ( genMnemonic
  , mnemonicToFile
  , mnemonicFromFile
  ) where

import Relude

import Cardano.Mnemonic
  ( MkMnemonicError
  , Mnemonic
  , entropyToMnemonic
  , genEntropy
  , mkMnemonic
  , mnemonicToText
  )
import Path (File, Path, toFilePath)

genMnemonic ∷ IO (Mnemonic 24)
genMnemonic = entropyToMnemonic <$> genEntropy @256

-- >>> genMnemonic >>= mnemonicToFile $(mkRelFile "test/data/mnemonic24.txt")
mnemonicToFile ∷ Path absRel File → Mnemonic 24 → IO ()
mnemonicToFile path mnemonic =
  writeFileText (toFilePath path) (unwords (mnemonicToText mnemonic) <> "\n")

mnemonicFromFile
  ∷ MonadIO m
  ⇒ Path absRel File
  → m (Either (MkMnemonicError 8) (Mnemonic 24))
mnemonicFromFile path =
  mkMnemonic @24 . words . decodeUtf8
    <$> readFileBS (toFilePath path)
