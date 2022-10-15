-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.MN.Corpus
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
corpus = (testContext {locale = makeLocale MN Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples =
  concat
    [ examples
        (simple Millilitre 250 Nothing)
        [ "250 миллилитр",
          "250мл",
          "250 мл"
        ],
      examples
        (simple Litre 2 Nothing)
        [ "2 литр",
          "2 л",
          "хоёр литр"
        ],
      examples
        (simple Litre 1 Nothing)
        [ "1 литр",
          "нэг литр",
          "1л"
        ],
      examples
        (simple Gallon 3 Nothing)
        [ "3 галлон"
        ],
      examples
        (simple Hectolitre 3 Nothing)
        [ "3 гектолитр",
          "3 гл",
          "3гл"
        ]
    ]
