module Yare.App.Types
  ( Config (..)
  , Services (..)
  ) where

import Relude

import Data.Tagged (Tagged)
import Ouroboros.Network.Magic (NetworkMagic)
import Path (Abs, File, Path)
import Yare.Chain.Types (ChainPoint, ChainTip)
import Yare.Node.Socket (NodeSocket)
import Yare.Utxo (Utxo)

-- | An application configuration
type Config ∷ Type
data Config = Config
  { apiHttpPort ∷ Word16
  , nodeSocket ∷ NodeSocket
  , networkMagic ∷ NetworkMagic
  , mnemonicFile ∷ Tagged "mnemonic" (Path Abs File)
  , syncFrom ∷ Maybe ChainPoint -- TODO: let app decide it based on the persistent state
  }

-- | Application services
type Services ∷ Type
data Services = Services
  { serveUtxo ∷ IO Utxo
  , serveTip ∷ IO ChainTip
  , deployScript ∷ IO ()
  }
