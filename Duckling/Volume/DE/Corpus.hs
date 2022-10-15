-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.DE.Corpus (corpus) where

import Data.String
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.Volume.Types
import Prelude

corpus :: Corpus
corpus = (testContext {locale = makeLocale DE Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples =
  concat
    [ examples
        (simple Litre 1 Nothing)
        [ "1 liter"
        --, "ein liter"
        ],
      examples
        (simple Litre 2 Nothing)
        [ "2 liter",
          "2l"
        ],
      examples
        (simple Litre 1000 Nothing)
        [ "1000 liter",
          "tausend liter"
        ],
      examples
        (simple Litre 0.5 Nothing)
        [ "halber liter",
          "ein halber liter"
        ],
      examples
        (simple Litre 0.25 Nothing)
        [ "viertel liter",
          "ein viertel liter"
        ],
      examples
        (simple Millilitre 1 Nothing)
        [ "ein milliliter",
          "ein ml",
          "1ml"
        ],
      examples
        (simple Millilitre 250 Nothing)
        [ "250 milliliter",
          "250ml",
          "250 ml"
        ],
      examples
        (simple Hectolitre 3 Nothing)
        [ "3 hektoliter"
        ],
      examples
        (between Litre (100, 1000) Nothing)
        [ "zwischen 100 und 1000 litern",
          "100-1000 liter",
          "von 100 bis 1000 l",
          "100 - 1000 l"
        ],
      examples
        (between Litre (2, 7) Nothing)
        [ "etwa 2 -7 l",
          "~2-7 liter",
          "von 2 bis 7 l",
          "zwischen 2,0 l und ungefähr 7,0 l",
          "zwischen 2l und etwa 7l",
          "2 - ~7 liter"
        ],
      examples
        (under Hectolitre 2 Nothing)
        [ "nicht mehr als 2 hektoliter",
          "höchstens zwei hektoliter",
          "unter 2 hektolitern",
          "weniger als 2 hektoliter"
        ],
      examples
        (above Millilitre 4 Nothing)
        [ "mehr als 4 ml",
          "wenigstens 4,0 ml",
          "über vier milliliter",
          "mindestens vier ml"
        ]
    ]
