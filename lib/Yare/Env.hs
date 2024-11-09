{- | Env (Environment) is an extensible record contining
globally available immutable values.
-}
module Yare.Env
  ( Env
  , Envᵣ
  , initialize
  , Tracersᵣ
  ) where

import Yare.Prelude

import Cardano.Api (NetworkMagic)
import Control.Concurrent.STM.TQueue (newTQueueIO)
import Control.Exception (throwIO)
import Control.Tracer (Tracer)
import Data.HList (HAppendListR, hAppendList)
import Fmt.Orphans ()
import Path (toFilePath)
import Path.IO (doesFileExist)
import Yare.Address (Addresses)
import Yare.Address qualified as Addresses
import Yare.App.State qualified as Yare
import Yare.App.Types (StorageMode (..))
import Yare.App.Types qualified as Yare
import Yare.Chain.Types (MnemonicPath)
import Yare.Query qualified as Query
import Yare.Storage (StorageMgr)
import Yare.Storage qualified as Storage
import Yare.Submitter qualified as Submitter
import Yare.Tracers (Tracersᵣ, tracers)

type Envᵣ =
  Query.Q
    : Submitter.Q
    : StorageMgr IO Yare.State
    : Addresses
    : HAppendListR Yare.Configᵣ Tracersᵣ

type Env = HList Envᵣ

initialize ∷ Yare.Config → IO Env
initialize config = do
  queryQueue ← newTQueueIO
  submitQueue ← newTQueueIO
  let netMagic = look @NetworkMagic config
      mnemonicFile = look @MnemonicPath config
  addresses ←
    Addresses.deriveFromMnemonic netMagic mnemonicFile
      >>= either throwIO pure
  volatileStorage ← Storage.inMemory Yare.initialState
  let dbFile = lookTagged @"database" config
  durableStorage ← do
    unlessM (doesFileExist dbFile) do
      writeFile (toFilePath dbFile) "" -- create an empty db file
    Storage.onDisk dbFile Yare.initialState
  storageManager ←
    Storage.storageManager
      Durable -- we always want to pick up durable state
      (look @(Tracer IO StorageMode) tracers)
      (Tagged @"volatile" volatileStorage)
      (Tagged @"durable" durableStorage)
  pure $
    queryQueue
      `strictHCons` submitQueue
      `strictHCons` storageManager
      `strictHCons` addresses
      `strictHCons` hAppendList config tracers
