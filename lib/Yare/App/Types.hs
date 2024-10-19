module Yare.App.Types
  ( Config
  , Configᵣ
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
import Network.Wai.Handler.Warp qualified as Warp
import Ouroboros.Network.Magic (NetworkMagic)
import Yare.Chain.Types (DatabasePath, MnemonicPath, SyncFrom)
import Yare.Node.Socket (NodeSocket)

type Configᵣ =
  '[ Warp.Port
   , NodeSocket
   , NetworkMagic
   , SyncFrom
   , MnemonicPath
   , DatabasePath
   ]

type Config = HList Configᵣ

data NetworkInfo era = NetworkInfo
  { network ∷ Network
  , systemStart ∷ SystemStart
  , epochInfo ∷ LedgerEpochInfo
  , currentEra ∷ BabbageEraOnwards era
  , protocolParameters ∷ LedgerProtocolParameters era
  }
