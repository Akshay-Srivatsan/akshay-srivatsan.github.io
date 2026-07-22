{-# LANGUAGE OverloadedStrings #-}

module Site.Config where

import Data.Map.Strict qualified as M
import Data.Text qualified as T

baseUrl :: String
baseUrl = "https://aks.io"

authorName :: String
authorName = "Akshay Srivatsan"

currentYear :: String
currentYear = "2026"

defaultImage :: String
defaultImage = "/assets/img/portrait-small.jpg"

transliterationFiles :: M.Map T.Text FilePath
transliterationFiles =
  M.fromList
    [ ("ta", "transliterate/ta.yaml"),
      ("sa", "transliterate/sa.yaml")
    ]

-- These are written in each language's source representation.  The switcher
-- runs them through that language's transliterator before displaying them.
languageEndonyms :: M.Map T.Text T.Text
languageEndonyms =
  M.fromList
    [ ("en", "English"),
      ("la", "Latina"),
      ("sa", "saṃskṛtam"),
      ("ta", "tamiḻ")
    ]
