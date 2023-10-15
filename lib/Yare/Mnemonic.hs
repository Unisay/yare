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
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Oops (CouldBe, Variant)
import Control.Monad.Oops qualified as Oops
import Path (File, Path, toFilePath)

genMnemonic ∷ IO (Mnemonic 24)
genMnemonic = entropyToMnemonic <$> genEntropy @256

-- >>> genMnemonic >>= mnemonicToFile $(mkRelFile "test/data/mnemonic24.txt")
mnemonicToFile ∷ Path absRel File → Mnemonic 24 → IO ()
mnemonicToFile path mnemonic =
  writeFileText (toFilePath path) (unwords (mnemonicToText mnemonic) <> "\n")

mnemonicFromFile
  ∷ ( MonadIO m
    , MonadError (Variant e) m
    , e `CouldBe` MkMnemonicError 8
    )
  ⇒ Path absRel File
  → m (Mnemonic 24)
mnemonicFromFile path = do
  mnemonic ← readFileBS (toFilePath path)
  mkMnemonic @24 (words (decodeUtf8 mnemonic)) & Oops.hoistEither
