-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.EN.Corpus
  ( corpus,
    latentCorpus,
  )
where

import Data.String
import Duckling.Quantity.Types
import Duckling.Resolve (Options (..))
import Duckling.Testing.Types
import Prelude

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

latentCorpus :: Corpus
latentCorpus = (testContext, testOptions {withLatent = True}, latentExamples)
  where
    latentExamples =
      concat
        [ examples
            (simple Unnamed 4 Nothing)
            [ "around 4",
              "four",
              "~ four"
            ],
          examples
            (simple Unnamed 38.5 Nothing)
            [ "about 38.5"
            ],
          examples
            (simple Unnamed 5 (Just "bananas"))
            [ "5 bananas"
            ]
        ]

allExamples :: [Example]
allExamples =
  concat
    [ examples
        (simple Pound 2 (Just "meat"))
        [ "two pounds of meat"
        ],
      examples
        (simple Pound 2 (Just "minced meat"))
        [ "two pounds of minced meat"
        ],
      examples
        (simple Gram 2 Nothing)
        [ "2 grams",
          "0.002 kg",
          "2 g.",
          "2 gs",
          "2/1000 kilograms",
          "2000 milligrams",
          "2,000 milligrams"
        ],
      examples
        (simple Gram 1000 Nothing)
        [ "a kilogram",
          "a kg",
          "1 k.g.",
          "1 k.gs",
          "1000 gs"
        ],
      examples
        (simple Pound 1 Nothing)
        [ "a Pound",
          "1 lb",
          "a lb"
        ],
      examples
        (simple Ounce 2 Nothing)
        [ "2 ounces",
          "2oz"
        ],
      examples
        (simple Cup 3 (Just "sugar"))
        [ "3 Cups of sugar",
          "3 Cups of SugAr"
        ],
      examples
        (simple Cup 1 (Just "sugar"))
        [ "1 cup sugar"
        ],
      examples
        (simple Cup 0.75 Nothing)
        [ "3/4 cup",
          "0.75 cup",
          ".75 cups"
        ],
      examples
        (simple Gram 500 (Just "strawberries"))
        [ "500 grams of strawberries",
          "500g of strawberries",
          "0.5 kilograms of strawberries",
          "0.5 kg of strawberries",
          "500000mg of strawberries"
        ],
      examples
        (between Gram (100, 1000) (Just "strawberries"))
        [ "100-1000 gram of strawberries",
          "between 100 and 1000 grams of strawberries",
          "from 100 to 1000 g of strawberries",
          "100 - 1000 g of strawberries"
        ],
      examples
        (between Gram (2, 7) Nothing)
        [ "around 2 -7 g",
          "~2-7 grams",
          "from 2 to 7 g",
          "between 2.0 g and about 7.0 g",
          "between 0.002 kg and about 0.007 kg",
          "2 - ~7 grams"
        ],
      examples
        (under Pound 6 (Just "meat"))
        [ "less than six pounds of meat",
          "no more than 6 lbs of meat",
          "below 6.0 pounds of meat",
          "at most six pounds of meat"
        ],
      examples
        (above Cup 2 Nothing)
        [ "exceeding 2 Cups",
          "at least two Cups",
          "over 2 Cups",
          "more than 2 Cups"
        ],
      examples
        (above Ounce 4 (Just "chocolate"))
        [ "exceeding 4 oz of chocolate",
          "at least 4.0 oz of chocolate",
          "over four ounces of chocolate",
          "more than four ounces of chocolate"
        ],
      examples
        (simple Clove 2 Nothing)
        [ "2 cloves"
        ],
      examples
        (simple Clove 1 (Just "garlic"))
        [ "1 clove of garlic"
        ],
      examples
        (simple Sniff 2 Nothing)
        [ "2 sniffs",
          "2 pinches"
        ],
      examples
        (simple Sniff 1 (Just "salt"))
        [ "1 Sniff of salt",
          "1 pinch of salt",
          "pinch of salt"
        ],
      examples
        (simple (Custom "Sprig") 1 (Just "thyme"))
        [ "1 sprig of thyme"
        ],
      examples
        (simple (Custom "Sprig") 2 (Just "thyme"))
        [ "2 sprigs of thyme"
        ],
      examples
        (simple (Custom "Bunch") 1 (Just "thyme"))
        [ "1 bunch of thyme"
        ],
      examples
        (simple (Custom "Bunch") 2 (Just "thyme"))
        [ "2 bunches of thyme"
        ],
      examples
        (simple (Custom "Handful") 1 (Just "thyme"))
        [ "1 handful of thyme"
        ],
      examples
        (simple (Custom "Handful") 2 (Just "thyme"))
        [ "2 handfuls of thyme"
        ],
      examples
        (simple (Custom "Slice") 1 (Just "thyme"))
        [ "1 slice of thyme"
        ],
      examples
        (simple (Custom "Slice") 2 (Just "thyme"))
        [ "2 slices of thyme"
        ],
      examples
        (simple (Custom "Splash") 1 (Just "olive oil"))
        [ "1 splash of olive oil",
          "a touch of olive oil",
          "1 dash of olive oil"
        ],
      examples
        (simple (Custom "Splash") 2 (Just "olive oil"))
        [ "2 splashes of olive oil",
          "2 touches of olive oil",
          "2 dashes of olive oil"
        ],
      examples
        (simple (Custom "Piece") 1 (Just "thyme"))
        [ "1 piece of thyme"
        ],
      examples
        (simple (Custom "Piece") 2 (Just "thyme"))
        [ "2 pieces of thyme"
        ],
      examples
        (simple (Custom "Pot") 1 (Just "honey"))
        [ "1 pot of honey"
        ],
      examples
        (simple (Custom "Pot") 2 (Just "honey"))
        [ "2 pots of honey"
        ],
      examples
        (simple (Custom "Package") 1 (Just "rice"))
        [ "1 package of rice"
        ],
      examples
        (simple (Custom "Package") 2 (Just "rice"))
        [ "2 packages of rice"
        ],
      examples
        (simple (Custom "Bag") 1 (Just "bread"))
        [ "1 bag of bread"
        ],
      examples
        (simple (Custom "Bag") 2 (Just "bread"))
        [ "2 bags of bread"
        ]
    ]
