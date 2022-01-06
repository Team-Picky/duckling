-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.FR.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Quantity.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale FR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (simple Cup 2 (Just "café"))
             [ "2 tasses de café"
             ]
  , examples (simple Cup 1 Nothing)
             [ "une Tasse"
             ]
  , examples (simple Tablespoon 3 (Just "sucre"))
             [ "3 Cuillères à soupe de sucre"
             , "3 Cuillèr à soupe de sucre"
             , "3 Cuiller à soupe de sucre"
             , "3 C. à soupe de sucre"
             , "3 C.às. de sucre"
             , "3 C.s. de sucre"
             , "3 Cas de sucre"
             , "3 Càs de sucre"
             ]
  , examples (simple Tablespoon 3 (Just "sucre blonde"))
             [ "3 Cuillères à soupe de sucre blonde"
             ]
  , examples (simple Teaspoon  3 (Just "sucre"))
             [ "3 Cuillères à cafe de sucre"
             , "3 Cuillères à café de sucre"
             , "3 Cuiller à café de sucre"
             , "3 C. à café de sucre"
             , "3 C.àc. de sucre"
             , "3 C.c. de sucre"
             , "3 Cac de sucre"
             , "3 Càc de sucre"
             ]
  , examples (simple Teaspoon 3 (Just "sucre blonde"))
             [ "3 Cuillères à cafe de sucre blonde"
             , "3 Cuillères à café de sucre blonde"
             ]
  , examples (simple Gram 2000 Nothing)
             [ "2 kg"
             , "2,0 kg"
             , "2 kilogramme"
             , "2 kilo"
             , "2000 gramme"
             , "2000 grammes"
             , "2000 g"
             , "deux mille gramme"
             , "2000000 mg"
             , "2000000 milligramme"
             , "2000000 milligrammes"
             ]
  , examples (simple Cup 1 (Just "sucre"))
             [ "1 tasse sucre"
             , "1 tasse de sucre"
             ]
  , examples (simple Cup 1 (Just "fraises fraîches"))
             [ "1 tasse fraises fraîches"
             , "1 tasse de fraises fraîches"
             ]
  , examples (simple Cup 3 (Just "sucre"))
             [ "3 tasses sucre"
             , "3 tasses de sucre"
             ]
  , examples (simple Cup 0.75 Nothing)
             [ "3/4 tasse"
             , "0,75 tasse"
             , ",75 tasse"
             ]
  , examples (simple Gram 500 (Just "fraises"))
             [ "500 gramme fraises"
             , "500g fraises"
             , "0,5 kilogramme fraises"
             , "0,5 kg fraises"
             , "500000mg fraises"
             ]
  , examples (between Gram (100,1000) (Just "fraises"))
              [ "100-1000 grammes fraises"
              , "entre 100 et 1000 grammes fraises"
              , "de 100 a 1000 g fraises"
              , "de 100 à 1000 g fraises"
              , "100 - 1000 g fraises"
              ]
  , examples (between Gram (2,7) Nothing)
              [ "~2-7 grammes"
              , "de 2 a 7 g"
              , "entre 2,0 g et environ 7,0 g"
              , "entre 0,002 kg et environ 0,007 kg"
              , "2 - ~7 grammes"
              ]
  , examples (simple Clove 2 Nothing)
             [ "2 gousses"
             ]
  , examples (simple Clove 1 (Just "aile"))
             [ "1 gousse d'aile"
             ]
  , examples (simple Sniff 2 Nothing)
             [ "2 pinces"
             ]
  , examples (simple Sniff 1 (Just "sel"))
             [ "1 pincée de sel"
             , "une pincée de sel"
             ]
  ]
