-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.AR.Corpus (corpus) where

import Data.String
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.Volume.Types
import Prelude

corpus :: Corpus
corpus = (testContext {locale = makeLocale AR Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples =
  concat
    [ examples
        (simple Millilitre 250 Nothing)
        [ "250 مل",
          "250مل",
          "250 ميليلتر",
          "250 ملي لتر"
        ],
      examples
        (simple Litre 2 Nothing)
        [ "2 لتر",
          "لتران",
          "لترين"
        ],
      examples
        (simple Gallon 3 Nothing)
        [ "3 غالون",
          "3 جالون",
          "3 غالونات",
          "3 جالونات"
        ],
      examples
        (simple Hectolitre 3 Nothing)
        [ "3 هكتوليتر",
          "3 هكتو ليتر"
        ],
      examples
        (simple Litre 0.5 Nothing)
        [ "نصف لتر",
          "نص لتر"
        ],
      examples
        (simple Litre 0.25 Nothing)
        [ "ربع لتر"
        ],
      examples
        (simple Litre 1.5 Nothing)
        [ "لتر ونصف",
          "لتر و نص",
          "لتر و نصف",
          "لتر ونص"
        ],
      examples
        (simple Litre 1.25 Nothing)
        [ "لتر وربع",
          "لتر و ربع"
        ]
    ]
