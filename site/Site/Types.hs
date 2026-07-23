{-# LANGUAGE OverloadedStrings #-}

module Site.Types where

import Data.Aeson (Value (..), (.!=), (.:), (.:?))
import Data.Scientific (FPFormat (Fixed), Scientific, floatingOrInteger, formatScientific)
import Data.Text qualified as T
import Data.Yaml qualified as Y
import System.FilePath ((</>))

data OutputLanguage
  = Default
  | LanguageTag T.Text
  deriving (Eq, Ord, Show)

data Style = Abugida | Alphabet
  deriving (Eq, Show)

instance Y.FromJSON Style where
  parseJSON = Y.withText "Style" $ \style -> case style of
    "abugida" -> pure Abugida
    "alphabet" -> pure Alphabet
    other -> fail $ "unknown script style: " <> T.unpack other

data Script = Script
  { scriptName :: T.Text,
    scriptStyle :: Style,
    scriptVirama :: T.Text,
    scriptVowels :: [[T.Text]],
    scriptDiacritics :: [[T.Text]],
    scriptModifiers :: [[T.Text]],
    scriptConsonants :: [[T.Text]],
    scriptPunctuation :: [[T.Text]],
    scriptDigits :: [T.Text]
  }
  deriving (Show)

instance Y.FromJSON Script where
  parseJSON = Y.withObject "Script" $ \o -> do
    name <- o .: "name" >>= parseTextScalar
    style <- o .: "style"
    virama <- o .:? "virama" .!= String "" >>= parseTextScalar
    vowels <- o .: "vowels" >>= parseTextTable
    diacritics <- o .:? "diacritics" .!= [] >>= parseTextTable
    modifiers <- o .:? "modifiers" .!= [[String ""]] >>= parseTextTable
    consonants <- o .:? "consonants" .!= [] >>= parseTextTable
    punctuation <- o .:? "punctuation" .!= [] >>= parseTextTable
    digits <- o .:? "digits" .!= [] >>= traverse parseTextScalar
    pure $ Script name style virama vowels diacritics modifiers consonants punctuation digits

parseTextTable :: [[Value]] -> Y.Parser [[T.Text]]
parseTextTable = traverse (traverse parseTextScalar)

parseTextScalar :: Value -> Y.Parser T.Text
parseTextScalar value =
  case value of
    String valueText -> pure valueText
    Number n -> pure $ T.pack $ scalarNumberText n
    Bool True -> pure "y"
    Bool False -> pure "n"
    Null -> pure ""
    _ -> fail "expected scalar text in transliteration table"

scalarNumberText :: Scientific -> String
scalarNumberText n =
  case (floatingOrInteger n :: Either Double Integer) of
    Right integer -> show integer
    Left _ -> formatScientific Fixed Nothing n

routeFor :: FilePath -> OutputLanguage -> FilePath
routeFor logicalPath language =
  case (logicalPath, language) of
    ("", Default) -> "index.html"
    ("", LanguageTag tag) -> T.unpack tag <> ".html"
    (_, Default) -> logicalPath </> "index.html"
    (_, LanguageTag tag) -> logicalPath </> T.unpack tag <> ".html"

urlFor :: FilePath -> OutputLanguage -> String
urlFor logicalPath language =
  case language of
    Default -> if null logicalPath then "/" else "/" <> logicalPath <> "/"
    LanguageTag tag -> "/" <> routeFor logicalPath (LanguageTag tag)

requestedTag :: OutputLanguage -> T.Text
requestedTag Default = "en"
requestedTag (LanguageTag tag) = tag

primaryLanguage :: T.Text -> T.Text
primaryLanguage = T.takeWhile (/= '-')
