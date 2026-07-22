{-# LANGUAGE OverloadedStrings #-}

module Site.Config where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Yaml ((.:))
import Data.Yaml qualified as Y

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

data Locale = Locale
  { localeName :: T.Text,
    localeStrings :: M.Map T.Text T.Text,
    localeScripts :: M.Map T.Text T.Text
  }
  deriving (Show)

instance Y.FromJSON Locale where
  parseJSON = Y.withObject "Locale" $ \object ->
    Locale
      <$> object .: "name"
      <*> object .: "strings"
      <*> object .: "scripts"

type Locales = M.Map T.Text Locale

loadLocales :: FilePath -> IO Locales
loadLocales path = do
  bytes <- BS.readFile path
  case Y.decodeEither' bytes of
    Left err -> fail $ Y.prettyPrintParseException err
    Right locales -> pure locales
