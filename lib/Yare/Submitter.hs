module Yare.Submitter
  ( TxSubmitCont (..)
  , TxSubmitResult
  , Q
  , client
  , submit
  ) where

import Yare.Prelude hiding (atomically)

import Cardano.Api (TxInMode)
import Cardano.Api qualified as Api
import Control.Concurrent.Class.MonadSTM (TQueue, readTQueue, writeTQueue)
import Control.Exception (throwIO)
import Control.Monad.Class.MonadSTM (MonadSTM (atomically))
import Ouroboros.Consensus.Cardano.Block (CardanoApplyTxErr, StandardCrypto)
import Ouroboros.Consensus.Ledger.SupportsMempool (GenTx)
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
  ( LocalTxClientStIdle (..)
  , LocalTxSubmissionClient (..)
  , SubmitResult (..)
  )
import Text.Pretty.Simple (pShow)
import Text.Show (show)
import Yare.Chain.Block (StdCardanoBlock)

type TxSubmitResult ∷ Type
type TxSubmitResult = SubmitResult (CardanoApplyTxErr StandardCrypto)

type TxSubmitCont ∷ (Type → Type) → Type
data TxSubmitCont m = TxSubmitCont TxInMode (TxSubmitResult → m ())

type Q ∷ Type
type Q = TQueue IO (TxSubmitCont IO)

client
  ∷ Q
  → LocalTxSubmissionClient
      (GenTx StdCardanoBlock)
      (CardanoApplyTxErr StandardCrypto)
      IO
      a
client txQ = LocalTxSubmissionClient (waitForTxToSubmit txQ)

waitForTxToSubmit
  ∷ Q
  → IO
      ( LocalTxClientStIdle
          (GenTx StdCardanoBlock)
          (CardanoApplyTxErr StandardCrypto)
          IO
          a
      )
waitForTxToSubmit txQ = do
  TxSubmitCont txAnyEra onResult ← atomically (readTQueue txQ)
  pure $ SendMsgSubmitTx (Api.toConsensusGenTx txAnyEra) \txResult → do
    onResult txResult
    waitForTxToSubmit txQ

{- | Submit a transaction to the network.
Throws 'TxSubmissionException' if the transaction submission fails.
-}
submit ∷ Q → TxInMode → IO ()
submit submitQ txInMode = do
  res ← newEmptyMVar
  atomically . writeTQueue submitQ $ TxSubmitCont txInMode \case
    SubmitSuccess → putMVar res Nothing
    SubmitFail err → putMVar res (Just err)
  takeMVar res >>= maybe pass (throwIO . TxSubmissionException)

newtype TxSubmissionException
  = TxSubmissionException (CardanoApplyTxErr StandardCrypto)
  deriving anyclass (Exception)

instance Show TxSubmissionException where
  show (TxSubmissionException err) =
    "Transaction submission failed:\n" <> toString (pShow err)
