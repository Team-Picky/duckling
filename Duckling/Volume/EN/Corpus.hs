-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.EN.Corpus (corpus) where

import Data.String
import Duckling.Testing.Types
import Duckling.Volume.Types
import Prelude

corpus :: Corpus
corpus = (testContext, testOptions, allExamples)

allExamples :: [Example]
allExamples =
  concat
    [ examples
        (simple Litre 1 Nothing)
        [ "1 liter",
          "1 litre",
          "one liter",
          "a liter"
        ],
      examples
        (simple Litre 2 Nothing)
        [ "2 liters",
          "2l"
        ],
      examples
        (simple Litre 1000 Nothing)
        [ "1000 liters",
          "thousand liters"
        ],
      examples
        (simple Litre 0.5 Nothing)
        [ "half liter",
          "half-litre",
          "half a liter"
        ],
      examples
        (simple Litre 0.25 Nothing)
        [ "quarter-litre",
          "fourth of liter"
        ],
      examples
        (simple Litre 0.02 Nothing)
        [ "2 centiliter",
          "2 centiliters",
          "2 centilitre",
          "2 centilitres",
          "2cl",
          "2 cl"
        ],
      examples
        (simple Litre 0.2 Nothing)
        [ "2 deciliter",
          "2 deciliters",
          "2 decilitre",
          "2 decilitres",
          "2dl",
          "2 dl"
        ],
      examples
        (simple Millilitre 1 Nothing)
        [ "one milliliter",
          "an ml",
          "a millilitre"
        ],
      examples
        (simple Millilitre 250 Nothing)
        [ "250 milliliters",
          "250 millilitres",
          "250ml",
          "250mls",
          "250 ml"
        ],
      examples
        (simple Gallon 3 Nothing)
        [ "3 gallons",
          "3 gal",
          "3gal",
          "around three gallons"
        ],
      examples
        (simple Gallon 0.5 Nothing)
        [ "0.5 gals",
          "1/2 gallon",
          "half a gallon"
        ],
      examples
        (simple Gallon 0.1 Nothing)
        [ "0.1 gallons",
          "tenth of a gallon"
        ],
      examples
        (simple Hectolitre 3 Nothing)
        [ "3 hectoliters"
        ],
      examples
        (between Litre (100, 1000) Nothing)
        [ "between 100 and 1000 liters",
          "100-1000 liters",
          "from 100 to 1000 l",
          "100 - 1000 l"
        ],
      examples
        (between Litre (2, 7) Nothing)
        [ "around 2 -7 l",
          "~2-7 liters",
          "from 2 to 7 l",
          "between 2.0 l and about 7.0 l",
          "between 2l and about 7l",
          "2 - ~7 litres"
        ],
      examples
        (under Gallon 6 Nothing)
        [ "less than six gallons",
          "under six gallon",
          "no more than 6 gals",
          "below 6.0gal",
          "at most six gallons"
        ],
      examples
        (above Hectolitre 2 Nothing)
        [ "exceeding 2 hectoliters",
          "at least two hectolitres",
          "over 2 hectolitre",
          "more than 2 hectoliter"
        ],
      examples
        (above Millilitre 4 Nothing)
        [ "exceeding 4 ml",
          "at least 4.0 ml",
          "over four milliliters",
          "more than four mls"
        ]
    ]
