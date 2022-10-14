-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Quantity.NL.Corpus
  ( corpus,
  )
where

import Data.String
import Duckling.Locale
import Duckling.Quantity.Types
import Duckling.Resolve
import Duckling.Testing.Types
import Prelude

context :: Context
context = testContext {locale = makeLocale NL Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

allExamples :: [Example]
allExamples =
  concat
    [ examples
        (simple Gram 2 Nothing)
        [ "2 gram",
          "0,002 kg",
          "0,002 kilo",
          "2/1000 kilogram",
          "2000 milligram"
        ],
      examples
        (simple Gram 1000 Nothing)
        [ "1 kg",
          "1,0 kg",
          "1 kilogram",
          "1 kilo",
          "1000 gram",
          "1000 g",
          "1000 gr",
          "duizend gram",
          "duizend gr",
          "2,0 pond",
          "10 ons",
          "1000000 mg",
          "1000000 milligram"
        ],
      examples
        (simple Cup 1 (Just "suiker"))
        [ "1 kopje suiker"
        ],
      examples
        (simple Cup 1 (Just "blonde suiker"))
        [ "1 kopje blonde suiker"
        ],
      examples
        (simple Cup 3 (Just "suiker"))
        [ "3 kopjes suiker"
        ],
      examples
        (simple Cup 0.75 Nothing)
        [ "3/4 kopje",
          "0,75 kopje",
          ",75 kopje"
        ],
      examples
        (simple Gram 500 (Just "aardbeien"))
        [ "500 gram aardbeien",
          "500g aardbeien",
          "0,5 kilogram aardbeien",
          "0,5 kg aardbeien",
          "5 ons aardbeien",
          "1 pond aardbeien",
          "500000mg aardbeien"
        ],
      examples
        (between Gram (100, 1000) (Just "aardbeien"))
        [ "100-1000 gram aardbeien",
          "tussen 100 en 1000 gram aardbeien",
          "van 100 tot 1000 g aardbeien",
          "tussen 1 ons en 10 ons aardbeien",
          "100 - 1000 g aardbeien"
        ],
      examples
        (between Gram (2, 7) Nothing)
        [ "~2-7 gram",
          "van 2 tot 7 g",
          "tussen 2,0 g en ongeveer 7,0 g",
          "tussen 0,002 kg en ongeveer 0,007 kg",
          "2 - ~7 gram"
        ],
      examples
        (simple Clove 2 Nothing)
        [ "2 teentjes"
        ],
      examples
        (simple Clove 1 (Just "look"))
        [ "1 teentje look"
        ],
      examples
        (simple Sniff 2 Nothing)
        [ "2 snuifjes"
        ],
      examples
        (simple Sniff 1 (Just "zout"))
        [ "1 snuifje zout",
          "een snuifje zout"
        ],
      examples
        (simple Teaspoon 1 (Just "suiker"))
        [ "1 theelepel suiker",
          "1 tl suiker",
          "1 kl suiker",
          "1 koffielepel suiker"
        ],
      examples
        (simple Teaspoon 2 (Just "suiker"))
        [ "2 theelepels suiker",
          "2 tl suiker",
          "2 kl suiker",
          "2 tls suiker",
          "2 kls suiker",
          "2 koffielepels suiker"
        ],
      examples
        (simple (Custom "Sprig") 1 (Just "tijm"))
        [ "1 takje tijm"
        ],
      examples
        (simple (Custom "Sprig") 2 (Just "tijm"))
        [ "2 takjes tijm"
        ],
      examples
        (simple (Custom "Bunch") 1 (Just "tijm"))
        [ "1 bosje tijm"
        ],
      examples
        (simple (Custom "Bunch") 2 (Just "tijm"))
        [ "2 bosjes tijm"
        ],
      examples
        (simple (Custom "Handful") 1 (Just "tijm"))
        [ "1 handvol tijm",
          "1 handje tijm"
        ],
      examples
        (simple (Custom "Handful") 2 (Just "tijm"))
        [ "2 handjes tijm"
        ],
      examples
        (simple (Custom "Slice") 1 (Just "tijm"))
        [ "1 sneetje tijm"
        ],
      examples
        (simple (Custom "Slice") 2 (Just "tijm"))
        [ "2 sneetjes tijm"
        ],
      examples
        (simple (Custom "Piece") 1 (Just "brood"))
        [ "1 stuk brood",
          "1 stukje brood"
        ],
      examples
        (simple (Custom "Piece") 2 (Just "brood"))
        [ "2 stukken brood",
          "2 stukjes brood"
        ],
      examples
        (simple (Custom "Splash") 1 (Just "olijfolie"))
        [ "1 vleug olijfolie",
          "1 vleugje olijfolie",
          "1 scheut olijfolie",
          "1 scheutje olijfolie",
          "scheutje olijfolie",
          "scheutjes olijfolie"
        ],
      examples
        (simple (Custom "Splash") 2 (Just "olijfolie"))
        [ "2 vleugen olijfolie",
          "2 vleugjes olijfolie",
          "2 scheuten olijfolie",
          "2 scheutjes olijfolie"
        ]
    ]
