-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.FR.Rules (rules) where

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
  [ ("<latent vol> ml", "(m(l|illilitres?))", TVolume.Millilitre),
    ("<latent vol> cl", "(c(l|entilitres?))", TVolume.Litre),
    ("<vol> deciliters", "(d(l|ecilitres?))", TVolume.Litre),
    ("<vol> hectoliters", "(h(l|ectolitres?))", TVolume.Hectolitre),
    ("<vol> liters", "(l(itres?)?)", TVolume.Litre),
    ("<latent vol> gallon", "(gal(l?ons?)?)", TVolume.Gallon)
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
fractions =
  [ ("quart", "quart de", 1 / 4),
    ("half", "demi-?", 1 / 2)
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
          regex "(?:(\\S+(?: +\\S+)*) *(?<! au goût)$)"
        ],
      prod = \case
        (Token Volume vd : Token RegexMatch (GroupMatch (product : _)) : _) ->
          Just . Token Volume $ withProduct (Text.toLower product) vd
        _ -> Nothing
    }

rulePrecision :: Rule
rulePrecision =
  Rule
    { name = "environ <volume>",
      pattern =
        [ regex "\\~|environ|exacte",
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
          regex "(?:(\\S+(?: +\\S+)*)(?: +au +goût))"
        ],
      prod = \case
        (Token Volume volume : Token RegexMatch (GroupMatch (product : _)) : _) ->
          Just . Token Volume $ withProduct (Text.toLower product) volume
        _ -> Nothing
    }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral =
  Rule
    { name = "<volume> - <volume>",
      pattern =
        [ Predicate isPositive,
          regex "\\-",
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
    { name = "<volume> - <volume>",
      pattern =
        [ Predicate isSimpleVolume,
          regex "\\-",
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
    { name = "moins de/au plus/au maximum <volume>",
      pattern =
        [ regex "moins de|au plus|au maximum",
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
    { name = "plus de/au moins/au minimum <volume>",
      pattern =
        [ regex "plus de|au moins|au minimum",
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
