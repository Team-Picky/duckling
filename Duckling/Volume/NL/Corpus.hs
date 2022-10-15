-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.NL.Corpus (corpus) where

import Data.String
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.Volume.Types
import Prelude

corpus :: Corpus
corpus = (testContext {locale = makeLocale NL Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples =
  concat
    [ examples
        (simple Millilitre 250 Nothing)
        [ "250 milliliter",
          "250 milliliters",
          "250ml",
          "250 ml"
        ],
      examples
        (simple Litre 2 Nothing)
        [ "2 liter",
          "2 liters",
          "2l",
          "2 l"
        ],
      examples
        (simple Litre 0.02 Nothing)
        [ "2 centiliter",
          "2 centiliters",
          "2cl",
          "2 cl"
        ],
      examples
        (simple Litre 0.2 Nothing)
        [ "2 deciliter",
          "2 deciliters",
          "2dl",
          "2 dl"
        ],
      examples
        (simple Hectolitre 3 Nothing)
        [ "3 hectoliter",
          "3 hectoliters",
          "3 hl"
        ],
      examples
        (simple Hectolitre 3 (Just "melk"))
        [ "3 hl melk naar smaak"
        ],
      examples
        (simple Litre 0.5 Nothing)
        [ "halve liter"
        ],
      examples
        (between Litre (100, 1000) Nothing)
        [ "tussen 100 en 1000 liters",
          "100-1000 liters",
          "van 100 tot 1000 l",
          "100 - 1000 l"
        ],
      examples
        (between Litre (2, 7) Nothing)
        [ "ongeveer 2 -7 l",
          "~2-7 liters",
          "van 2 tot 7 l",
          "tussen 2 l en dichtbij 7 l",
          "tussen 2l en exact 7l",
          "2 - ~7 liters"
        ],
      examples
        (under Litre 6 Nothing)
        [ "minder dan zes liters",
          "onder zes liter",
          "niet meer dan 6 liters",
          "max 6l",
          "maximaal zes liters",
          "hoogstens zes liters",
          "ten hoogste zes liters",
          "lager dan zes liters",
          "kleiner dan zes liters"
        ],
      examples
        (above Hectolitre 2 Nothing)
        [ "boven 2 hectoliters",
          "min twee hectoliter",
          "minimaal 2 hectoliters",
          "ten minste 2 hectoliters",
          "meer dan 2 hectoliter",
          "groter dan 2 hectoliter",
          "hoger dan 2 hectoliter",
          "niet minder dan 2 hectoliter",
          "minstens 2 hectoliter"
        ]
    ]
