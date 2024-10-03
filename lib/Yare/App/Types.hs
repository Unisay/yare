module Yare.App.Types
  ( Config (..)
  , NetworkInfo (..)
  ) where

import Yare.Prelude

import Cardano.Api.Ledger (Network)
import Cardano.Api.Shelley
  ( BabbageEraOnwards
  , LedgerEpochInfo
  , LedgerProtocolParameters
  , SystemStart
  )
import Data.Tagged (Tagged)
import Network.Wai.Handler.Warp qualified as Warp
import Ouroboros.Network.Magic (NetworkMagic)
import Path (Abs, File, Path)
import Yare.Chain.Point (ChainPoint)
import Yare.Node.Socket (NodeSocket)

-- | An application configuration
data Config = Config
  { apiHttpPort ∷ Warp.Port
  , nodeSocket ∷ NodeSocket
  , networkMagic ∷ NetworkMagic
  , mnemonicFile ∷ Tagged "mnemonic" (Path Abs File)
  , syncFrom ∷ Maybe ChainPoint -- TODO: let app decide it based on the persistent state
  }

data NetworkInfo era = NetworkInfo
  { network ∷ Network
  , systemStart ∷ SystemStart
  , epochInfo ∷ LedgerEpochInfo
  , currentEra ∷ BabbageEraOnwards era
  , protocolParameters ∷ LedgerProtocolParameters era
  }
