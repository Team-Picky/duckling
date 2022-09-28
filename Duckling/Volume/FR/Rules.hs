-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.FR.Rules
  ( rules ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.String
import Data.Text (Text)
import Prelude

import Duckling.Dimensions.Types
import Duckling.Types
import Duckling.Regex.Types
import Duckling.Volume.Helpers
import Duckling.Numeral.Helpers (isPositive)
import qualified Duckling.Volume.Types as TVolume
import qualified Duckling.Numeral.Types as TNumeral

volumes :: [(Text, String, TVolume.Unit)]
volumes = [ ("<latent vol> ml"    , "(m(l|illilitres?))" , TVolume.Millilitre)
          , ("<latent vol> cl"    , "(c(l|entilitres?))" , TVolume.Litre)
          , ("<vol> deciliters"  , "(d(l|ecilitres?))"    , TVolume.Litre)
          , ("<vol> hectoliters"  , "(h(l|ectolitres?))"    , TVolume.Hectolitre)
          , ("<vol> liters"       , "(l(itres?)?)"     , TVolume.Litre)
          , ("<latent vol> gallon", "(gal(l?ons?)?)"   , TVolume.Gallon)
          ]

opsMap :: HashMap Text (Double -> Double)
opsMap =
  HashMap.fromList
    [ ("centilitre", (/ 100)),
      ("centilitres", (/ 100)),
      ("cl", (/ 100)),
      ("decilitre", (/ 10)),
      ("decilitres", (/ 10)),
      ("dl", (/ 10))
    ]

ruleNumeralVolumes :: [Rule]
ruleNumeralVolumes = map go volumes
  where
    go :: (Text, String, TVolume.Unit) -> Rule
    go (name, regexPattern, u) =
      Rule
        { name = name,
          pattern = [Predicate isPositive, regex regexPattern],
          prod = \case
            ( Token Numeral nd
                : Token RegexMatch (GroupMatch (match : _))
                : _
              ) -> Just . Token Volume $ volume u value
                where
                  value = getValue opsMap match $ TNumeral.value nd
            _ -> Nothing
        }

fractions :: [(Text, String, Double)]
fractions = [ ("quart", "quart de", 1/4)
            , ("half", "demi-?", 1/2)
            ]

rulesFractionalVolume :: [Rule]
rulesFractionalVolume = flatmap go fractions
  where
    go :: (Text, String, Double) -> [Rule]
    go (fractionName, fractionRegexPattern, f) =
      map goVolumes volumes
        where
          goVolumes :: (Text, String, TVolume.Unit) -> Rule
          goVolumes (name, regexPattern, u) = Rule
            { name = fractionName <> " " <> name
            , pattern =
              [  regex fractionRegexPattern
              , regex regexPattern
              ]
            , prod = \case
              (_:_:_) ->
                Just . Token Volume $ volume u f
              _ -> Nothing
            }

rules :: [Rule]
rules =
  [
  ]
  ++ ruleNumeralVolumes
  ++ rulesFractionalVolume
