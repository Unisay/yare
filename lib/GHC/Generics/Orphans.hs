{-# OPTIONS_GHC -Wno-orphans #-}

module GHC.Generics.Orphans () where

import Cardano.Api qualified as CApi
import GHC.Generics (Generic)

deriving stock instance Generic CApi.TxIn
