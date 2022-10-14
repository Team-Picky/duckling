-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.NL.Rules
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
import Duckling.Quantity.Helpers
import Duckling.Quantity.Types (QuantityData (..))
import qualified Duckling.Quantity.Types as TQuantity
import Duckling.Regex.Types (GroupMatch (..))
import Duckling.Types
import Prelude

quantities :: [(Text, String, TQuantity.Unit)]
quantities =
  [ ("<quantity> kopje", "(kopjes?)", TQuantity.Cup),
    ("<quantity> grams", "(g((r)?(am)?)?)", TQuantity.Gram),
    ("<quantity> milligrams", "((m(illi)?)(g(ram)?))", TQuantity.Gram),
    ("<quantity> kilograms", "((k(ilo)?)(g(ram)?)?)", TQuantity.Gram),
    ("<quantity> pond", "(pond(je(s)?)?)", TQuantity.Gram),
    ("<quantity> ons", "(ons(je(s)?)?)", TQuantity.Gram),
    ("<quantity> theelepel", "(((t(hee)?)|(k(offie)?))l(epel)?(s)?)", TQuantity.Teaspoon),
    ("<quantity> eetlepel", "(e(et)?l(epel)?(s)?)", TQuantity.Tablespoon),
    ("<quantity> teen", "((tenen)|(teen(tje(s)?)?))", TQuantity.Clove),
    ("<quantity> snuifje", "((snuiven)|(snuif(je(s)?)?))", TQuantity.Sniff),
    ("<quantity> takje", "(tak(je(s)?)?)", TQuantity.Custom "Sprig"),
    ("<quantity> bosje", "((bos(je(s)?)?)|(bussel(s|(tjes?)?)))", TQuantity.Custom "Bunch"),
    ("<quantity> handvol", "(hand((vol)|(je(s)?))?)", TQuantity.Custom "Handful"),
    ("<quantity> sneetje", "(snee(tje(s)?)?|sneden)", TQuantity.Custom "Slice"),
    ("<quantity> scheutje", "((scheut((en)|(jes?))?)|(vleug((en)|(jes?))?))", TQuantity.Custom "Splash"),
    ("<quantity> stuk", "(stuk((ken)|(jes?))?)", TQuantity.Custom "Piece"),
    ("<quantity> pot", "(pot((ten)|(jes?))?)", TQuantity.Custom "Pot"),
    ("<quantity> pak", "(pak((ken)|(jes?))?)", TQuantity.Custom "Package"),
    ("<quantity> zak", "(zak((ken)|(jes?))?)", TQuantity.Custom "Bag"),
    ("<quantity> stronk", "(stronk((en)|(jes?))?)", TQuantity.Custom "Stump"),
    ("<quantity> stengel", "(stengel((s)|(tjes?))?)", TQuantity.Custom "Stalk"),
    ("<quantity> blok", "(blok((ken)|(jes?))?)", TQuantity.Custom "Cube")
  ]

opsMap :: HashMap Text (Double -> Double)
opsMap =
  HashMap.fromList
    [ ("milligram", (/ 1000)),
      ("mg", (/ 1000)),
      ("kilo", (* 1000)),
      ("kilogram", (* 1000)),
      ("kg", (* 1000)),
      ("pond", (* 500)),
      ("ons", (* 100))
    ]

ruleNumeralQuantities :: [Rule]
ruleNumeralQuantities = map go quantities
  where
    go :: (Text, String, TQuantity.Unit) -> Rule
    go (name, regexPattern, u) =
      Rule
        { name = name,
          pattern = [Predicate isPositive, regex regexPattern],
          prod = \case
            ( Token Numeral nd
                : Token RegexMatch (GroupMatch (match : _))
                : _
              ) -> Just . Token Quantity $ quantity u value
                where
                  value = getValue opsMap match $ TNumeral.value nd
            _ -> Nothing
        }

ruleAQuantity :: [Rule]
ruleAQuantity = map go quantities
  where
    go :: (Text, String, TQuantity.Unit) -> Rule
    go (name, regexPattern, u) =
      Rule
        { name = name,
          pattern = [regex ("(?:een )?" ++ regexPattern)],
          prod = \case
            ( Token RegexMatch (GroupMatch (match : _))
                : _
              ) -> Just . Token Quantity $ quantity u $ getValue opsMap match 1
            _ -> Nothing
        }

ruleQuantityOfProduct :: Rule
ruleQuantityOfProduct =
  Rule
    { name = "<quantity> product",
      pattern =
        [ dimension Quantity,
          regex "(\\S+(?: +\\S+)*)"
        ],
      prod = \case
        (Token Quantity qd : Token RegexMatch (GroupMatch (product : _)) : _) ->
          Just . Token Quantity $ withProduct (Text.toLower product) qd
        _ -> Nothing
    }

rulePrecision :: Rule
rulePrecision =
  Rule
    { name = "ongeveer|plm|plusminus <quantity>",
      pattern =
        [ regex "\\~|precies|exact|ongeveer|bijna|ongeveer",
          dimension Quantity
        ],
      prod = \case
        (_ : token : _) -> Just token
        _ -> Nothing
    }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral =
  Rule
    { name = "tussen|van <numeral> en|tot <quantity>",
      pattern =
        [ regex "tussen|van",
          Predicate isPositive,
          regex "tot|en",
          Predicate isSimpleQuantity
        ],
      prod = \case
        ( _
            : Token Numeral NumeralData {TNumeral.value = from}
            : _
            : Token
                Quantity
                QuantityData
                  { TQuantity.value = Just to,
                    TQuantity.unit = Just u,
                    TQuantity.aproduct = Nothing
                  }
            : _
          )
            | from < to ->
              Just . Token Quantity . withInterval (from, to) $ unitOnly u
        _ -> Nothing
    }

ruleIntervalBetween :: Rule
ruleIntervalBetween =
  Rule
    { name = "rond|tussen|van <quantity> tot|en <quantity>",
      pattern =
        [ regex "tussen|van",
          Predicate isSimpleQuantity,
          regex "en|tot",
          Predicate isSimpleQuantity
        ],
      prod = \case
        ( _
            : Token
                Quantity
                QuantityData
                  { TQuantity.value = Just from,
                    TQuantity.unit = Just u1,
                    TQuantity.aproduct = Nothing
                  }
            : _
            : Token
                Quantity
                QuantityData
                  { TQuantity.value = Just to,
                    TQuantity.unit = Just u2,
                    TQuantity.aproduct = Nothing
                  }
            : _
          )
            | from < to && u1 == u2 ->
              Just . Token Quantity . withInterval (from, to) $ unitOnly u1
        _ -> Nothing
    }

ruleIntervalNumeralDash :: Rule
ruleIntervalNumeralDash =
  Rule
    { name = "<numeral> - <quantity>",
      pattern =
        [ Predicate isPositive,
          regex "\\-",
          Predicate isSimpleQuantity
        ],
      prod = \case
        ( Token Numeral NumeralData {TNumeral.value = from}
            : _
            : Token
                Quantity
                QuantityData
                  { TQuantity.value = Just to,
                    TQuantity.unit = Just u,
                    TQuantity.aproduct = Nothing
                  }
            : _
          )
            | from < to ->
              Just . Token Quantity . withInterval (from, to) $ unitOnly u
        _ -> Nothing
    }

ruleIntervalDash :: Rule
ruleIntervalDash =
  Rule
    { name = "<quantity> - <quantity>",
      pattern =
        [ Predicate isSimpleQuantity,
          regex "\\-",
          Predicate isSimpleQuantity
        ],
      prod = \case
        ( Token
            Quantity
            QuantityData
              { TQuantity.value = Just from,
                TQuantity.unit = Just u1,
                TQuantity.aproduct = Nothing
              }
            : _
            : Token
                Quantity
                QuantityData
                  { TQuantity.value = Just to,
                    TQuantity.unit = Just u2,
                    TQuantity.aproduct = Nothing
                  }
            : _
          )
            | from < to && u1 == u2 ->
              Just . Token Quantity . withInterval (from, to) $ unitOnly u1
        _ -> Nothing
    }

ruleIntervalMax :: Rule
ruleIntervalMax =
  Rule
    { name = "minder dan/hoogstens/op zijn hoogst/maximaal/hooguit <quantity>",
      pattern =
        [ regex "minder dan|hoogstens|hooguit|maximaal|op zijn hoogst",
          Predicate isSimpleQuantity
        ],
      prod = \case
        ( _
            : Token
                Quantity
                QuantityData
                  { TQuantity.value = Just to,
                    TQuantity.unit = Just u,
                    TQuantity.aproduct = Nothing
                  }
            : _
          ) -> Just . Token Quantity . withMax to $ unitOnly u
        _ -> Nothing
    }

ruleIntervalMin :: Rule
ruleIntervalMin =
  Rule
    { name = "meer dan/minstens/op zijn minst <quantity>",
      pattern =
        [ regex "meer dan|minstens|minimaal|op zijn minst|minder dan",
          Predicate isSimpleQuantity
        ],
      prod = \case
        ( _
            : Token
                Quantity
                QuantityData
                  { TQuantity.value = Just from,
                    TQuantity.unit = Just u,
                    TQuantity.aproduct = Nothing
                  }
            : _
          ) -> Just . Token Quantity . withMin from $ unitOnly u
        _ -> Nothing
    }

rules :: [Rule]
rules =
  [ ruleQuantityOfProduct,
    ruleIntervalMin,
    ruleIntervalMax,
    ruleIntervalBetweenNumeral,
    ruleIntervalBetween,
    ruleIntervalNumeralDash,
    ruleIntervalDash,
    rulePrecision
  ]
    ++ ruleNumeralQuantities
    ++ ruleAQuantity
