-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Volume.ZH.Corpus (corpus) where

import Data.String
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Types
import Duckling.Volume.Types
import Prelude

corpus :: Corpus
corpus = (testContext {locale = makeLocale ZH Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples =
  concat
    [ examples
        (simple Litre 1 Nothing)
        [ "1升",
          "一升",
          "一公升",
          "1L"
        ],
      examples
        (simple Litre 2 Nothing)
        [ "2升",
          "兩升",
          "兩公升",
          "2L"
        ],
      examples
        (simple Litre 1000 Nothing)
        [ "1000公升",
          "一千公升"
        ],
      examples
        (simple Litre 0.5 Nothing)
        [ "半公升",
          "0.5L",
          "0.5L左右"
        ],
      examples
        (simple Litre 0.25 Nothing)
        [ "四分一升",
          "四分之一公升"
        ],
      examples
        (simple Millilitre 1 Nothing)
        [ "1毫升",
          "1ml",
          "一毫升",
          "1cc"
        ],
      examples
        (simple Millilitre 250 Nothing)
        [ "250毫升",
          "二百五十毫升",
          "250ml",
          "250cc"
        ],
      examples
        (simple Gallon 3 Nothing)
        [ "3加侖",
          "三加侖"
        ],
      examples
        (simple Gallon 0.5 Nothing)
        [ "0.5加侖",
          "半加侖",
          "二分一加侖"
        ],
      examples
        (simple Gallon 0.1 Nothing)
        [ "0.1加侖",
          "零點一加侖"
        ],
      examples
        (between Litre (2, 3) Nothing)
        [ "二至三公升",
          "2-3L",
          "2~3公升",
          "兩到三升",
          "兩升到三升"
        ],
      examples
        (under Gallon 6 Nothing)
        [ "最多六個加侖",
          "六加侖以下"
        ],
      examples
        (above Millilitre 4 Nothing)
        [ "至少四ml",
          "最少四毫升",
          "四毫升或以上",
          "起碼四毫升"
        ],
      examples
        (simple Millilitre 5 Nothing)
        [ "一茶匙",
          "三分一湯匙"
        ],
      examples
        (between Litre (60, 70) Nothing)
        [ "六十到七十L"
        ]
    ]
