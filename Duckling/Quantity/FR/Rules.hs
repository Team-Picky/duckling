-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.FR.Rules
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
  [ ("<quantity> tasse", "(tasses?( de)?)", TQuantity.Cup),
    ("<quantity> grams", "(g(ramme(s)?)?)", TQuantity.Gram),
    ("<quantity> milligrams", "((m(illi)?)(g(ramme(s)?)?))", TQuantity.Gram),
    ("<quantity> kilograms", "((k(ilo)?)(g(ramme(s)?)?)?)", TQuantity.Gram),
    ("<quantity> theelepel à café", "((c((uill?(e|è)re?s?)|\\.)? (a|à) caf(e|é))|(c\\.?(a|à)?c\\.?))", TQuantity.Teaspoon),
    ("<quantity> cuillère à soupe", "((c((uill?(e|è)re?s?)|\\.)? (a|à) soupe)|(c\\.?(a|à)?s\\.?))", TQuantity.Tablespoon),
    ("<quantity> gousse", "(gousse(s)?)", TQuantity.Clove),
    ("<quantity> pincée", "(pinc((es)|(er)|((e|é)e(s)?)))", TQuantity.Sniff),
    ("<quantity> tige", "((brin(dille)?(s)?)|(branche(s)?)|(tige(s)?))", TQuantity.Custom "Sprig"),
    ("<quantity> botte", "((botte(s)?)|(bouquet(s)?)|(tas))", TQuantity.Custom "Bunch"),
    ("<quantity> poigné", "(poign(é|e)(e)?(s)?)", TQuantity.Custom "Handful"),
    ("<quantity> tranche", "(tranche(s)?)", TQuantity.Custom "Slice"),
    ("<quantity> filet", "((filet)|((e|é)claboussure)|(touche)|(goutte))", TQuantity.Custom "Splash")
  ]

opsMap :: HashMap Text (Double -> Double)
opsMap =
  HashMap.fromList
    [ ("milligramme", (/ 1000)),
      ("milligrammes", (/ 1000)),
      ("mg", (/ 1000)),
      ("kilo", (* 1000)),
      ("kilogramme", (* 1000)),
      ("kilogrammes", (* 1000)),
      ("kg", (* 1000))
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
          pattern = [regex ("(une? )?" ++ regexPattern)],
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
          regex "(?:(?:de +)|(?:d'))?(\\S+(?: +\\S+)*)"
        ],
      prod = \case
        (Token Quantity qd : Token RegexMatch (GroupMatch (product : _)) : _) ->
          Just . Token Quantity $ withProduct (Text.toLower product) qd
        _ -> Nothing
    }

rulePrecision :: Rule
rulePrecision =
  Rule
    { name = "environ <quantity>",
      pattern =
        [ regex "\\~|environ|exacte",
          dimension Quantity
        ],
      prod = \case
        (_ : token : _) -> Just token
        _ -> Nothing
    }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral =
  Rule
    { name = "de|entre <numeral> à|et <quantity>",
      pattern =
        [ regex "de|entre",
          Predicate isPositive,
          regex "a|à|et",
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
    { name = "de|entre <quantity> à|et <quantity>",
      pattern =
        [ regex "de|entre",
          Predicate isSimpleQuantity,
          regex "a|à|et",
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
    { name = "moins de/au plus/au maximum <quantity>",
      pattern =
        [ regex "moins de|au plus|au maximum",
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
    { name = "plus de/au moins/au minimum <quantity>",
      pattern =
        [ regex "plus de|au moins|au minimum",
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
