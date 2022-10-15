-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.CA.Corpus (corpus) where

import Data.String
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.Volume.Types
import Prelude

corpus :: Corpus
corpus = (testContext {locale = makeLocale CA Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples =
  concat
    [ examples
        (simple Millilitre 250 Nothing)
        [ "250 mililitres",
          "250ml",
          "250 ml"
        ],
      examples
        (simple Litre 2 Nothing)
        [ "2 litres"
        ],
      examples
        (simple Gallon 3 Nothing)
        [ "3 galó"
        ],
      examples
        (simple Hectolitre 3 Nothing)
        [ "3 hectolitres"
        ],
      examples
        (simple Litre 0.5 Nothing)
        [ "mig litre"
        ]
    ]
