-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Volume.Types where

import Control.DeepSeq
import Data.Aeson
import qualified Data.HashMap.Strict as H
import Data.Hashable
import Data.Text (Text)
import qualified Data.Text as Text
import Duckling.Resolve (Resolve (..))
import GHC.Generics
import Prelude

data Unit
  = Gallon
  | Hectolitre
  | Litre
  | Centilitre
  | Millilitre
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance ToJSON Unit where
  toJSON = String . Text.toLower . Text.pack . show

data VolumeData = VolumeData
  { value :: Maybe Double,
    unit :: Maybe Unit,
    aproduct :: Maybe Text,
    minValue :: Maybe Double,
    maxValue :: Maybe Double
  }
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

instance Resolve VolumeData where
  type ResolvedValue VolumeData = VolumeValue
  resolve _ _ VolumeData {value = Just v, unit = Just u, aproduct = p} =
    Just (simple u v p, False)
  resolve
    _
    _
    VolumeData
      { value = Nothing,
        unit = Just u,
        aproduct = p,
        minValue = Just from,
        maxValue = Just to
      } =
      Just (between u (from, to) p, False)
  resolve
    _
    _
    VolumeData
      { value = Nothing,
        unit = Just u,
        aproduct = p,
        minValue = Just v,
        maxValue = Nothing
      } =
      Just (above u v p, False)
  resolve
    _
    _
    VolumeData
      { value = Nothing,
        unit = Just u,
        aproduct = p,
        minValue = Nothing,
        maxValue = Just v
      } =
      Just (under u v p, False)
  resolve _ _ _ = Nothing

data IntervalDirection = Above | Under
  deriving (Eq, Generic, Hashable, Ord, Show, NFData)

data SingleValue = SingleValue
  { vUnit :: Unit,
    vValue :: Double,
    vProduct :: Maybe Text
  }
  deriving (Eq, Show)

instance ToJSON SingleValue where
  toJSON SingleValue {vUnit, vValue, vProduct} =
    object $
      [ "value" .= vValue,
        "unit" .= vUnit
      ]
        ++ ["product" .= p | Just p <- [vProduct]]

data VolumeValue
  = SimpleValue SingleValue
  | IntervalValue (SingleValue, SingleValue)
  | OpenIntervalValue (SingleValue, IntervalDirection)
  deriving (Show, Eq)

instance ToJSON VolumeValue where
  toJSON (SimpleValue value) = case toJSON value of
    Object o -> Object $ H.insert "type" (toJSON ("value" :: Text)) o
    _ -> Object H.empty
  toJSON (IntervalValue (from, to)) =
    object
      [ "type" .= ("interval" :: Text),
        "from" .= toJSON from,
        "to" .= toJSON to
      ]
  toJSON (OpenIntervalValue (from, Above)) =
    object
      [ "type" .= ("interval" :: Text),
        "from" .= toJSON from
      ]
  toJSON (OpenIntervalValue (to, Under)) =
    object
      [ "type" .= ("interval" :: Text),
        "to" .= toJSON to
      ]

-- -----------------------------------------------------------------
-- Value helpers

simple :: Unit -> Double -> Maybe Text -> VolumeValue
simple u v p = SimpleValue $ single u v p

between :: Unit -> (Double, Double) -> Maybe Text -> VolumeValue
between u (from, to) p = IntervalValue (single u from p, single u to p)

above :: Unit -> Double -> Maybe Text -> VolumeValue
above = openInterval Above

under :: Unit -> Double -> Maybe Text -> VolumeValue
under = openInterval Under

openInterval :: IntervalDirection -> Unit -> Double -> Maybe Text -> VolumeValue
openInterval direction u v p = OpenIntervalValue (single u v p, direction)

single :: Unit -> Double -> Maybe Text -> SingleValue
single u v p = SingleValue {vUnit = u, vValue = v, vProduct = p}
