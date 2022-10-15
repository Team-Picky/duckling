-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.EN.Rules (rules) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (isPositive)
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Regex.Types
import Duckling.Types
import Duckling.Volume.Helpers
import qualified Duckling.Volume.Types as TVolume
import Prelude

volumes :: [(Text, String, TVolume.Unit)]
volumes =
  [ ("<latent vol> ml", "(m(l(s?)|illilit(er|re)s?))", TVolume.Millilitre),
    ("<vol> hectoliters", "(h(l(s?)|ectolit(er|re)s?))", TVolume.Hectolitre),
    ("<vol> deciliters", "(d(l(s?)|ecilit(er|re)s?))", TVolume.Litre),
    ("<vol> centiliters", "(c(l(s?)|entilit(er|re)s?))", TVolume.Litre),
    ("<vol> liters", "(l(it(er|re)s?)?)", TVolume.Litre),
    ("<latent vol> gallon", "(gal((l?ons?)|s)?)", TVolume.Gallon)
  ]

opsMap :: HashMap Text (Double -> Double)
opsMap =
  HashMap.fromList
    [ ("centiliter", (/ 100)),
      ("centiliters", (/ 100)),
      ("centilitre", (/ 100)),
      ("centilitres", (/ 100)),
      ("cl", (/ 100)),
      ("deciliter", (/ 10)),
      ("deciliters", (/ 10)),
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
fractions =
  [ ("one", "an? ", 1),
    ("half", "half(-|(( of)?( a(n?))?))?", 1 / 2),
    ("third", "third(-|(( of)?( a(n?))?))?", 1 / 3),
    ("fourth", "(quarter|fourth)(-|(( of)?( a(n?))?))?", 1 / 4),
    ("fifth", "fifth(-|(( of)?( a(n?))?))?", 1 / 5),
    ("tenth", "tenth(-|(( of)?( a(n?))?))?", 1 / 10)
  ]

rulesFractionalVolume :: [Rule]
rulesFractionalVolume = flatmap go fractions
  where
    go :: (Text, String, Double) -> [Rule]
    go (fractionName, fractionRegexPattern, f) =
      map goVolumes volumes
      where
        goVolumes :: (Text, String, TVolume.Unit) -> Rule
        goVolumes (name, regexPattern, u) =
          Rule
            { name = fractionName <> " " <> name,
              pattern =
                [ regex fractionRegexPattern,
                  regex regexPattern
                ],
              prod = \case
                (_ : _ : _) ->
                  Just . Token Volume $ volume u f
                _ -> Nothing
            }

ruleVolumeOfProduct :: Rule
ruleVolumeOfProduct =
  Rule
    { name = "<volume> product",
      pattern =
        [ dimension Volume,
          regex "(?:(\\S+(?: +\\S+)*) *(?<! to taste)$)"
        ],
      prod = \case
        (Token Volume vd : Token RegexMatch (GroupMatch (product : _)) : _) ->
          Just . Token Volume $ withProduct (Text.toLower product) vd
        _ -> Nothing
    }

rulePrecision :: Rule
rulePrecision =
  Rule
    { name = "about <volume>",
      pattern =
        [ regex "\\~|exactly|precisely|about|approx(\\.|imately)?|close to|near( to)?|around|almost",
          dimension Volume
        ],
      prod = \case
        (_ : token : _) -> Just token
        _ -> Nothing
    }

ruleAddendum :: Rule
ruleAddendum =
  Rule
    { name = "<volume> au goût",
      pattern =
        [ dimension Volume,
          regex "(?:(\\S+(?: +\\S+)*)(?: +to +taste))"
        ],
      prod = \case
        (Token Volume volume : Token RegexMatch (GroupMatch (product : _)) : _) ->
          Just . Token Volume $ withProduct (Text.toLower product) volume
        _ -> Nothing
    }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral =
  Rule
    { name = "between|from <numeral> and|to <volume>",
      pattern =
        [ regex "between|from",
          Predicate isPositive,
          regex "to|and",
          Predicate isSimpleVolume
        ],
      prod = \case
        ( _
            : Token Numeral TNumeral.NumeralData {TNumeral.value = from}
            : _
            : Token
                Volume
                TVolume.VolumeData
                  { TVolume.value = Just to,
                    TVolume.unit = Just u
                  }
            : _
          )
            | from < to ->
              Just . Token Volume . withInterval (from, to) $ unitOnly u
        _ -> Nothing
    }

ruleIntervalBetween :: Rule
ruleIntervalBetween =
  Rule
    { name = "between|from <volume> to|and <volume>",
      pattern =
        [ regex "between|from",
          Predicate isSimpleVolume,
          regex "to|and",
          Predicate isSimpleVolume
        ],
      prod = \case
        ( _
            : Token
                Volume
                TVolume.VolumeData
                  { TVolume.value = Just from,
                    TVolume.unit = Just u1
                  }
            : _
            : Token
                Volume
                TVolume.VolumeData
                  { TVolume.value = Just to,
                    TVolume.unit = Just u2
                  }
            : _
          )
            | from < to && u1 == u2 ->
              Just . Token Volume . withInterval (from, to) $ unitOnly u1
        _ -> Nothing
    }

ruleIntervalMax :: Rule
ruleIntervalMax =
  Rule
    { name = "at most <volume>",
      pattern =
        [ regex "under|below|at most|(less|lower|not? more) than",
          Predicate isSimpleVolume
        ],
      prod = \case
        ( _
            : Token
                Volume
                TVolume.VolumeData
                  { TVolume.value = Just to,
                    TVolume.unit = Just u
                  }
            : _
          ) ->
            Just . Token Volume . withMax to $ unitOnly u
        _ -> Nothing
    }

ruleIntervalMin :: Rule
ruleIntervalMin =
  Rule
    { name = "more than <volume>",
      pattern =
        [ regex "over|above|exceeding|beyond|at least|(more|larger|bigger|heavier) than",
          Predicate isSimpleVolume
        ],
      prod = \case
        ( _
            : Token
                Volume
                TVolume.VolumeData
                  { TVolume.value = Just from,
                    TVolume.unit = Just u
                  }
            : _
          ) ->
            Just . Token Volume . withMin from $ unitOnly u
        _ -> Nothing
    }

rules :: [Rule]
rules =
  [ ruleVolumeOfProduct,
    rulePrecision,
    ruleIntervalBetweenNumeral,
    ruleIntervalBetween,
    ruleIntervalMax,
    ruleIntervalMin,
    ruleAddendum
  ]
    ++ ruleNumeralVolumes
    ++ rulesFractionalVolume
