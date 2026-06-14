{-# LANGUAGE OverloadedStrings #-}

module Site.Types where

import Data.Aeson (Value (..), (.:), (.:?), (.!=))
import Data.Scientific (FPFormat (Fixed), Scientific, floatingOrInteger, formatScientific)
import qualified Data.Text as T
import qualified Data.Yaml as Y

data Variant = Variant
    { variantCode :: String
    , variantLabel :: String
    , variantLang :: String
    , variantMapping :: Maybe T.Text
    }
    deriving (Eq, Show)

data PageSpec = PageSpec
    { pageSource :: FilePath
    , pageOutput :: FilePath
    , pageGroup :: String
    , pageLanguageLabel :: String
    , pageVariants :: [Variant]
    }
    deriving (Eq, Show)

data Style = Abugida | Alphabet
    deriving (Eq, Show)

instance Y.FromJSON Style where
    parseJSON = Y.withText "Style" $ \style -> case style of
        "abugida" -> pure Abugida
        "alphabet" -> pure Alphabet
        other -> fail $ "unknown script style: " <> T.unpack other

data Script = Script
    { scriptName :: T.Text
    , scriptStyle :: Style
    , scriptVirama :: T.Text
    , scriptVowels :: [[T.Text]]
    , scriptDiacritics :: [[T.Text]]
    , scriptModifiers :: [[T.Text]]
    , scriptConsonants :: [[T.Text]]
    , scriptPunctuation :: [[T.Text]]
    , scriptDigits :: [T.Text]
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
        String text -> pure text
        Number n -> pure $ T.pack $ scalarNumberText n
        Bool True -> pure "y"
        Bool False -> pure "n"
        Null -> pure ""
        _ -> fail "expected scalar text in transliteration table"

scalarNumberText :: Scientific -> String
scalarNumberText n =
    case (floatingOrInteger n :: Either Double Integer) of
        Right integer -> show (integer :: Integer)
        Left _ -> formatScientific Fixed Nothing n

type Mapping = T.Text
type TransliterationMap = [(T.Text, T.Text)]
