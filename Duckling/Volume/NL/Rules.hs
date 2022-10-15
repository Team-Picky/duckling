-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.NL.Rules
  ( rules,
  )
where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers
import Duckling.Numeral.Types (NumeralData (..))
import qualified Duckling.Numeral.Types as TNumeral
import Duckling.Regex.Types (GroupMatch (..))
import Duckling.Types
import Duckling.Volume.Helpers
import qualified Duckling.Volume.Types as TVolume
import Prelude

volumes :: [(Text, String, TVolume.Unit)]
volumes =
  [ ("<latent vol> ml", "(m(illi)?l(iters?)?)", TVolume.Millilitre),
    ("<vol> hl", "(h(ecto)?l(iters?)?)", TVolume.Hectolitre),
    ("<vol> centiliters", "(c(enti)?l(iters?)?)", TVolume.Litre),
    ("<vol> deciliters", "(d(eci)?l(iters?)?)", TVolume.Litre),
    ("<vol> liters", "(l(iters?)?)", TVolume.Litre)
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
fractions =
  [ ("half", "halve", 1 / 2),
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
          regex "(?:(\\S+(?: +\\S+)*) *(?<! naar smaak)$)"
        ],
      prod = \case
        (Token Volume vd : Token RegexMatch (GroupMatch (product : _)) : _) ->
          Just . Token Volume $ withProduct (Text.toLower product) vd
        _ -> Nothing
    }

rulePrecision :: Rule
rulePrecision =
  Rule
    { name = "ongeveer <volume>",
      pattern =
        [ regex "\\~|exact|precies|ongeveer|dichtbij|rond|bijna",
          dimension Volume
        ],
      prod = \case
        (_ : token : _) -> Just token
        _ -> Nothing
    }

ruleAddendum :: Rule
ruleAddendum =
  Rule
    { name = "<volume> naar smaak",
      pattern =
        [ dimension Volume,
          regex "(?:(\\S+(?: +\\S+)*)(?: +naar +smaak))"
        ],
      prod = \case
        (Token Volume volume : Token RegexMatch (GroupMatch (product : _)) : _) ->
          Just . Token Volume $ withProduct (Text.toLower product) volume
        _ -> Nothing
    }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral =
  Rule
    { name = "tussen|van <numeral> en|tot <volume>",
      pattern =
        [ regex "tussen|van",
          Predicate isPositive,
          regex "en|tot",
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
    { name = "tussen|van <volume> en|tot <volume>",
      pattern =
        [ regex "tussen|van",
          Predicate isSimpleVolume,
          regex "en|tot",
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
    { name = "maximaal <volume>",
      pattern =
        [ regex "onder|max(?:imaal)?|(?:ten )?hoogste(?:ns)?|(minder|kleiner|niet meer|lager) dan",
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
    { name = "minimaal <volume>",
      pattern =
        [ regex "boven|(?:ten )?min((?:imaal)|ste(?:ns)?)?|(meer|groter|niet minder|hoger) dan",
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
