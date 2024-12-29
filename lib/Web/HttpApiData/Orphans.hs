{-# OPTIONS_GHC -Wno-orphans #-}

module Web.HttpApiData.Orphans () where

import Yare.Prelude

import Cardano.Api
  ( AsType (AsAssetName, AsPolicyId)
  , AssetName
  , PolicyId
  , deserialiseFromRawBytesHex
  )
import Web.HttpApiData (FromHttpApiData, parseUrlPiece)

instance FromHttpApiData PolicyId where
  parseUrlPiece =
    first show . deserialiseFromRawBytesHex AsPolicyId . encodeUtf8

instance FromHttpApiData AssetName where
  parseUrlPiece =
    first show . deserialiseFromRawBytesHex AsAssetName . encodeUtf8
