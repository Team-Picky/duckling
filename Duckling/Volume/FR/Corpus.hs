-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.FR.Corpus (corpus) where

import Data.String
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.Volume.Types
import Prelude

corpus :: Corpus
corpus = (testContext {locale = makeLocale FR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples =
  concat
    [ examples
        (simple Millilitre 250 Nothing)
        [ "250 millilitres",
          "250ml",
          "250 ml"
        ],
      examples
        (simple Litre 0.25 Nothing)
        [ "25 centilitres",
          "25cl",
          "25 cl"
        ],
      examples
        (simple Litre 0.2 Nothing)
        [ "2 decilitres",
          "2dl",
          "2 dl"
        ],
      examples
        (simple Litre 2 Nothing)
        [ "2 litres"
        ],
      examples
        (simple Gallon 3 Nothing)
        [ "3 gallons",
          "3 gal"
        ],
      examples
        (simple Hectolitre 3 Nothing)
        [ "3 hectolitres"
        ],
      examples
        (simple Litre 0.5 Nothing)
        [ "demi-litre",
          "demi litre"
        ],
      examples
        (simple Litre 0.25 Nothing)
        [ "quart de litre"
        ],
      examples
        (simple Hectolitre 3 (Just "lait"))
        [ "3 hl lait au goût"
        ],
      examples
        (between Litre (100, 1000) Nothing)
        [ "100-1000 litres",
          "100 - 1000 l"
        ],
      examples
        (between Litre (2, 7) Nothing)
        [ "environ 2 -7 l",
          "~2-7 litres",
          "exacte 2 - 7 l",
          "2 - ~7 litres"
        ],
      examples
        (under Litre 6 Nothing)
        [ "moins de six litres",
          "au plus six litre",
          "au maximum 6 litres"
        ],
      examples
        (above Hectolitre 2 Nothing)
        [ "plus de 2 hectolitres",
          "au moins deux hectolitres",
          "au minimum 2 hectolitres"
        ]
    ]
