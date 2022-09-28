-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.NL.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Volume.Helpers
import qualified Duckling.Volume.Types as TVolume
import Duckling.Regex.Types (GroupMatch (..))
import Duckling.Types
import Prelude

volumes :: [(Text, String, TVolume.Unit)]
volumes = [ ("<latent vol> ml"    , "(m(illi)?l(iter)?)" , TVolume.Millilitre)
          , ("<vol> hl"           , "(h(ecto)?l(iter)?)"   , TVolume.Hectolitre)
          , ("<vol> centiliters"       , "(c(enti)?l(iters?)?)"    , TVolume.Litre)
          , ("<vol> deciliters"       , "(d(eci)?l(iters?)?)"    , TVolume.Litre)
          , ("<vol> liters"       , "(l(iters?)?)"    , TVolume.Litre)
          ]

opsMap :: HashMap Text (Double -> Double)
opsMap =
  HashMap.fromList
    [ ("centiliter", (/ 100)),
      ("centiliters", (/ 100)),
      ("cl", (/ 100)),
      ("deciliter", (/ 10)),
      ("deciliters", (/ 10)),
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
fractions = [ ("half", "halve", 1/2),
              ("een", "een", 1)
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
