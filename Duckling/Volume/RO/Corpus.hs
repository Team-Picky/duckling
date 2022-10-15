-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.RO.Corpus
  ( corpus,
  )
where

import Data.String
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.Volume.Types
import Prelude

corpus :: Corpus
corpus = (testContext {locale = makeLocale RO Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples =
  concat
    [ examples
        (simple Millilitre 250 Nothing)
        [ "250 mililitri",
          "250 de mililitri",
          "250ml",
          "250 ml",
          "250 de ml"
        ],
      examples
        (simple Litre 2 Nothing)
        [ "2 litri",
          "2 l",
          "2l"
        ],
      examples
        (simple Gallon 3 Nothing)
        [ "3 galoane",
          "3 gal"
        ],
      examples
        (simple Hectolitre 3 Nothing)
        [ "3 hectolitri"
        ],
      examples
        (simple Litre 0.5 Nothing)
        [ "jumatate de litru",
          "jumătate de litru"
        ],
      examples
        (simple Gallon 20 Nothing)
        [ "douazeci de galoane"
        ]
    ]
