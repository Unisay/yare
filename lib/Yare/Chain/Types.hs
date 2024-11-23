module Yare.Chain.Types
  ( ChainTip
  , BlockRef (..)
  , ChainPoint
  , LastIndexedBlock
  , MnemonicPath
  , DatabasePath
  , parseChainPoint
  , LedgerAddress
  , ledgerAddressToText
  ) where

import Yare.Prelude

import Cardano.Api.Shelley qualified as Api
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Address qualified as Ledger
import Cardano.Ledger.Crypto (StandardCrypto)
import Data.ByteString.Base16 qualified as B16
import Data.Maybe.Strict (StrictMaybe)
import Data.Text.Encoding qualified as Text
import Ouroboros.Network.Block (Tip)
import Path (Abs, File, Path)
import Yare.Chain.Block (StdCardanoBlock)
import Yare.Chain.Block.Reference (BlockRef (..))
import Yare.Chain.Point (ChainPoint, parseChainPoint)

type ChainTip = Tip StdCardanoBlock

type LedgerAddress = Addr StandardCrypto

type LastIndexedBlock = Tagged "last-indexed" (StrictMaybe BlockRef)

type MnemonicPath = Tagged "mnemonic" (Path Abs File)

type DatabasePath = Tagged "database" (Path Abs File)

ledgerAddressToText ∷ LedgerAddress → Text
ledgerAddressToText = \case
  bootstrapAddr@(AddrBootstrap _bootstrapAddr) →
    -- Bootstrap addresses are encoded in base16
    -- as BECH32 is not supported for Byron addresses.
    Text.decodeLatin1 . B16.encode $ Ledger.serialiseAddr bootstrapAddr
  Addr network cred stakeRef →
    -- Shelley addresses are encoded in BECH32
    -- using functionality from 'Cardano.Api.Shelley'.
    Api.serialiseToBech32 $ Api.ShelleyAddress network cred stakeRef
