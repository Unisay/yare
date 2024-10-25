module Yare.Tracers
  ( Tracers
  , Tracersᵣ
  , tracers
  , tracerTxId
  , tracerUtxo
  , tracerSync
  , tracerRollback
  ) where

import Yare.Prelude

import Cardano.Api (SlotNo (..), TxId)
import Control.Tracer.Extended
  ( Tracer
  , condTracing
  , debugTracer
  , lineTracer
  , withPrefix
  )
import Fmt (pretty)
import Fmt.Orphans ()
import Yare.Chain.Point (ChainPoint)
import Yare.Utxo (Utxo)

type Tracersᵣ =
  [ Tracer IO TxId
  , Tracer IO Utxo
  , Tracer IO SlotNo
  , Tracer IO ChainPoint
  ]

type Tracers = HList Tracersᵣ

tracers ∷ Tracers
tracers = tracerTxId .*. tracerUtxo .*. tracerSync .*. tracerRollback .*. HNil

tracerTxId ∷ Tracer IO TxId
tracerTxId = pretty >$< withPrefix "Tx Id" debugTracer

tracerUtxo ∷ Tracer IO Utxo
tracerUtxo = mappend "\n" . pretty >$< withPrefix "UTxO" debugTracer

tracerSync ∷ Tracer IO SlotNo
tracerSync =
  condTracing (\(SlotNo slot) → slot `mod` 100 == 0) $
    pretty >$< withPrefix "Slot" lineTracer

tracerRollback ∷ Tracer IO ChainPoint
tracerRollback = pretty >$< withPrefix "Rollback" debugTracer
