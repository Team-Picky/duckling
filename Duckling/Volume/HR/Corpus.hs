-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.HR.Corpus (corpus) where

import Data.String
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.Volume.Types
import Prelude

corpus :: Corpus
corpus = (testContext {locale = makeLocale HR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples =
  concat
    [ examples
        (simple Millilitre 250 Nothing)
        [ "250 mililitara",
          "250ml",
          "250 ml"
        ],
      examples
        (simple Litre 2 Nothing)
        [ "2 litre"
        ],
      examples
        (simple Gallon 3 Nothing)
        [ "3 galona",
          "3 gal"
        ],
      examples
        (simple Hectolitre 3 Nothing)
        [ "3 hektolitra"
        ],
      examples
        (simple Litre 0.5 Nothing)
        [ "pola litre"
        ]
    ]
