{-# LANGUAGE OverloadedStrings #-}

module Site.Transliterate where

import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Ord (Down (Down))
import qualified Data.Text as T
import qualified Data.Text.Normalize as TN
import qualified Data.Yaml as Y
import Site.Config (mappingPathForPage, replacementsFor)
import Site.Types
import System.FilePath (dropExtension, splitDirectories, takeDirectory, (</>))
import Text.HTML.TagSoup hiding (renderTags)
import qualified Text.HTML.TagSoup as TS

type MappingTable = M.Map T.Text T.Text
type MappingSet = M.Map T.Text MappingTable

loadMappingsForPage :: PageSpec -> IO MappingSet
loadMappingsForPage page =
    maybe (pure M.empty) loadMappings (mappingPathForPage page)

loadMappings :: FilePath -> IO MappingSet
loadMappings path = do
    bytes <- BS.readFile path
    scripts <- case Y.decodeAllEither' bytes of
        Left err -> fail $ Y.prettyPrintParseException err
        Right values -> pure values
    case scripts of
        [] -> pure M.empty
        source : targets ->
            pure $
                M.fromList
                    [ ("to_" <> scriptName target, makeMapping source target)
                    | target <- targets
                    ]

makeMapping :: Script -> Script -> MappingTable
makeMapping source target =
    normalizeMap $ case (scriptStyle source, scriptStyle target) of
        (Abugida, Abugida) -> mapAbugidas source target
        (Abugida, Alphabet) -> mapAbugidaAlphabet source target
        (Alphabet, Abugida) -> mapAlphabetAbugida source target
        (Alphabet, Alphabet) -> mapAlphabets source target

mapAbugidas, mapAbugidaAlphabet, mapAlphabetAbugida, mapAlphabets :: Script -> Script -> MappingTable
mapAbugidas source target =
    M.fromList $
        vowelPairs
            <> consonantPairs
            <> zipFlat (scriptPunctuation source) (scriptPunctuation target)
            <> zip (scriptDigits source) (scriptDigits target)
  where
    vowelPairs =
        [ (sv <> sm, tv <> tm)
        | (sv, tv) <- zip (flat scriptVowels source) (flat scriptVowels target)
        , (sm, tm) <- zip (flat scriptModifiers source) (flat scriptModifiers target)
        ]
    consonantPairs =
        concat
            [ [ (sc <> sd <> sm, tc <> td <> tm)
              | (sd, td) <- zip (flat scriptDiacritics source) (flat scriptDiacritics target)
              , (sm, tm) <- zip (flat scriptModifiers source) (flat scriptModifiers target)
              ]
                <> [(sc <> scriptVirama source, tc <> scriptVirama target)]
            | (sc, tc) <- zip (flat scriptConsonants source) (flat scriptConsonants target)
            ]

mapAbugidaAlphabet source target =
    M.fromList $
        vowelPairs
            <> consonantPairs
            <> zipFlat (scriptPunctuation source) (scriptPunctuation target)
            <> zip (scriptDigits source) (scriptDigits target)
  where
    vowelPairs =
        [ (sv <> sm, tv <> tm)
        | (sv, tv) <- zip (flat scriptVowels source) (flat scriptVowels target)
        , (sm, tm) <- zip (flat scriptModifiers source) (flat scriptModifiers target)
        ]
    consonantPairs =
        concat
            [ [ (sc <> sd <> sm, tc <> tv <> tm)
              | (sd, tv) <- zip (flat scriptDiacritics source) (flat scriptVowels target)
              , (sm, tm) <- zip (flat scriptModifiers source) (flat scriptModifiers target)
              ]
                <> [(sc <> scriptVirama source, tc)]
            | (sc, tc) <- zip (flat scriptConsonants source) (flat scriptConsonants target)
            ]

mapAlphabetAbugida source target =
    M.fromList $
        zip (flat scriptVowels source) (flat scriptVowels target)
            <> consonantPairs
            <> zipFlat (scriptPunctuation source) (scriptPunctuation target)
            <> zip (scriptDigits source) (scriptDigits target)
  where
    consonantPairs =
        concat
            [ [ (sc <> sv <> sm, tc <> td <> tm)
              | (sv, td) <- zip (flat scriptVowels source) (flat scriptDiacritics target)
              , (sm, tm) <- zip (flat scriptModifiers source) (flat scriptModifiers target)
              ]
                <> [(sc, tc <> scriptVirama target)]
            | (sc, tc) <- zip (flat scriptConsonants source) (flat scriptConsonants target)
            ]

mapAlphabets source target =
    M.fromList $
        zip (flat scriptVowels source) (flat scriptVowels target)
            <> zip (flat scriptModifiers source) (flat scriptModifiers target)
            <> zip (flat scriptConsonants source) (flat scriptConsonants target)
            <> zipFlat (scriptPunctuation source) (scriptPunctuation target)
            <> zip (scriptDigits source) (scriptDigits target)

flat :: (Script -> [[T.Text]]) -> Script -> [T.Text]
flat scriptField = concat . scriptField

zipFlat :: [[T.Text]] -> [[T.Text]] -> [(T.Text, T.Text)]
zipFlat a b = zip (concat a) (concat b)

normalizeMap :: MappingTable -> MappingTable
normalizeMap = M.fromList . fmap normalizePair . M.toList
  where
    normalizePair (k, v) = (normalize k, normalize v)

normalize :: T.Text -> T.Text
normalize = TN.normalize TN.NFC . T.replace "◌" "" . TN.normalize TN.NFD

transformFor :: PageSpec -> MappingSet -> Variant -> String -> String
transformFor page mappings variant =
    maybe id (transcribeString page mappings) (variantMapping variant)

transformHtml :: PageSpec -> MappingSet -> Variant -> String -> String
transformHtml page mappings variant body =
    TS.renderTags $ go [] $ parseTags body
  where
    maybeMapping = variantMapping variant >>= (`M.lookup` mappings)
    depthPrefix = concat $ replicate (outputDepth page variant) "../"
    go _ [] = []
    go stack (tag : tags) =
        case tag of
            TagOpen name attrs ->
                let skip = shouldSkip name attrs
                 in TagOpen name (rewriteAttrs depthPrefix attrs) : go (skip : stack) tags
            TagClose _ ->
                tag : go (drop 1 stack) tags
            TagText text
                | or stack -> tag : go stack tags
                | otherwise ->
                    TagText (T.unpack $ maybe (T.pack text) (\mapping -> transcribeText page mapping $ T.pack text) maybeMapping) : go stack tags
            _ -> tag : go stack tags

outputDepth :: PageSpec -> Variant -> Int
outputDepth page variant =
    length $ filter (not . null) $ splitDirectories $ takeDirectory $ outputFor page variant

rewriteAttrs :: String -> [(String, String)] -> [(String, String)]
rewriteAttrs prefix = fmap rewriteAttr
  where
    rewriteAttr (key, value)
        | key `elem` ["href", "src"] = (key, rewriteUrl prefix value)
        | otherwise = (key, value)

rewriteUrl :: String -> String -> String
rewriteUrl prefix value
    | null prefix = value
    | any (`startsWith` value) ["http://", "https://", "mailto:", "#", "/", "../"] = value
    | otherwise = prefix <> value

startsWith :: String -> String -> Bool
startsWith prefix s = take (length prefix) s == prefix

transcribeString :: PageSpec -> MappingSet -> T.Text -> String -> String
transcribeString page mappings mappingName text =
    case M.lookup mappingName mappings of
        Nothing -> text
        Just mapping -> T.unpack $ transcribeText page mapping $ T.pack text

transcribeText :: PageSpec -> MappingTable -> T.Text -> T.Text
transcribeText page mapping text =
    preserveBoundaryWhitespace text $
        fixTamilVariants
            . applyReplacements page
            . postprocessHindiTransliteration page
            . transcribeWithoutReplacements mapping
            . preprocessHindiSource page

preserveBoundaryWhitespace :: T.Text -> (T.Text -> T.Text) -> T.Text
preserveBoundaryWhitespace text transform
    | T.null middle = text
    | otherwise = leading <> transform middle <> trailing
  where
    (leading, withoutLeading) = T.span isSpace text
    (middle, trailing) = spanEndText isSpace withoutLeading

spanEndText :: (Char -> Bool) -> T.Text -> (T.Text, T.Text)
spanEndText predicate text =
    (T.reverse middleReversed, T.reverse trailingReversed)
  where
    (trailingReversed, middleReversed) = T.span predicate $ T.reverse text

transcribeWithoutReplacements :: MappingTable -> T.Text -> T.Text
transcribeWithoutReplacements mapping =
    trimOne
        . restoreSpacing endingChars False
        . restoreSpacing startingChars True
        . replaceAllLongest mapping
        . addBoundarySpacing
        . T.replace "\n" " "
  where
    startingChars = ["(", "—", "-", "\"", "“"]
    endingChars = [")", "—", "-", "\"", "“", ":", ".", "।", ","]
    addBoundarySpacing s =
        " "
            <> foldl (\acc c -> T.replace c (c <> " ") acc) s startingChars
            <> " "
    restoreSpacing chars isStarting s =
        foldr
            ( \c acc ->
                if isStarting
                    then T.replace (c <> " ") c acc
                    else T.replace (" " <> c) c acc
            )
            s
            chars
    trimOne s = fromMaybe s $ T.stripPrefix " " s >>= T.stripSuffix " "

applyReplacements :: PageSpec -> T.Text -> T.Text
applyReplacements page text =
    foldl replaceOne text (replacementsFor page)
  where
    replaceOne acc (from, to) = T.replace (T.replace "◌" "" from) (T.replace "◌" "" to) acc

preprocessHindiSource :: PageSpec -> T.Text -> T.Text
preprocessHindiSource page
    | pageSource page == "content/hindi.md" = deleteHindiSchwas
    | ".hi.md" `endsWith` pageSource page = deleteHindiSchwas
    | otherwise = id

postprocessHindiTransliteration :: PageSpec -> T.Text -> T.Text
postprocessHindiTransliteration page
    | pageSource page == "content/hindi.md" = shortenFinalHindiEnglishVowels
    | ".hi.md" `endsWith` pageSource page = shortenFinalHindiEnglishVowels
    | otherwise = id

endsWith :: String -> String -> Bool
endsWith suffix s = suffix == drop (length s - length suffix) s

deleteHindiSchwas :: T.Text -> T.Text
deleteHindiSchwas =
    restoreHindiPunctuation . T.unwords . fmap deleteWordSchwas . T.words . spaceHindiPunctuation

spaceHindiPunctuation :: T.Text -> T.Text
spaceHindiPunctuation text =
    foldl spaceOne text hindiPunctuation
  where
    spaceOne acc c = T.replace (T.singleton c) (" " <> T.singleton c <> " ") acc

restoreHindiPunctuation :: T.Text -> T.Text
restoreHindiPunctuation text =
    restorePunctuation hindiOpeningPunctuation hindiClosingPunctuation text

hindiPunctuation :: [Char]
hindiPunctuation = ".,()–-“”:।"

hindiOpeningPunctuation, hindiClosingPunctuation :: [Char]
hindiOpeningPunctuation = "(-“"
hindiClosingPunctuation = ".,)–”/:।"

deleteWordSchwas :: T.Text -> T.Text
deleteWordSchwas word
    | T.length word <= 1 = word
    | firstSyllableEnding == T.length word - 1 = word
    | otherwise = deleteInternalSchwas firstSyllableEnding $ deleteInfinitiveSchwa firstSyllableEnding $ deleteFinalSchwa word
  where
    firstSyllableEnding = findFirstSyllableEnding word

findFirstSyllableEnding :: T.Text -> Int
findFirstSyllableEnding word =
    go 0
  where
    len = T.length word
    go i
        | i >= len = len - 1
        | isHindiVocalic current = i
        | isHindiConsonant current && maybe False isHindiConsonant next = i
        | otherwise = go (i + 1)
      where
        current = T.index word i
        next = charAt word (i + 1)

deleteFinalSchwa :: T.Text -> T.Text
deleteFinalSchwa word
    | maybe False isHindiConsonant (lastChar word) && not (ending `elem` finalSchwaExceptions) = word <> hindiHalantText
    | otherwise = word
  where
    ending = T.drop (T.length word - 3) word
    finalSchwaExceptions = ["न्य", "त्र"]

deleteInfinitiveSchwa :: Int -> T.Text -> T.Text
deleteInfinitiveSchwa firstSyllableEnding word
    | T.isSuffixOf "ना" word || T.isSuffixOf "ने" word =
        let ending = T.drop (T.length word - 2) word
            beginning = T.take (T.length word - 2) word
         in if T.length beginning >= firstSyllableEnding && T.length beginning >= 2 && maybe False isHindiConsonant (lastChar beginning)
                then beginning <> hindiHalantText <> ending
                else word
    | otherwise = word

deleteInternalSchwas :: Int -> T.Text -> T.Text
deleteInternalSchwas firstSyllableEnding word =
    foldr maybeInsertHalant word [firstSyllableEnding - 2 .. T.length word]
  where
    maybeInsertHalant i acc =
        case (charAt acc i, charAt acc (i + 1), charAt acc (i + 2), charAt acc (i + 3)) of
            (Just a, Just b, Just c, d)
                | (isHindiVocalic a || isHindiConsonant a)
                    && isHindiConsonant b
                    && isHindiConsonant c
                    && c /= hindiNukta
                    && d /= Just hindiHalant ->
                    T.take (i + 2) acc <> hindiHalantText <> T.drop (i + 2) acc
            _ -> acc

shortenFinalHindiEnglishVowels :: T.Text -> T.Text
shortenFinalHindiEnglishVowels =
    cleanupHindiPostprocess . restoreEnglishPunctuation . T.unwords . fmap shortenWord . T.words . spaceEnglishPunctuation
  where
    shortenWord word
        | T.isSuffixOf "aa" word = T.dropEnd 2 word <> "a"
        | T.isSuffixOf "ee" word = T.dropEnd 2 word <> "i"
        | T.isSuffixOf "oo" word = T.dropEnd 2 word <> "u"
        | otherwise = word

cleanupHindiPostprocess :: T.Text -> T.Text
cleanupHindiPostprocess =
    collapseDigitPeriodSpaces . T.replace "- " "-"

collapseDigitPeriodSpaces :: T.Text -> T.Text
collapseDigitPeriodSpaces = T.pack . go . T.unpack
  where
    go (a : '.' : ' ' : b : rest)
        | isDigitChar a && isDigitChar b = a : '.' : b : go rest
    go (x : xs) = x : go xs
    go [] = []

isDigitChar :: Char -> Bool
isDigitChar c = c >= '0' && c <= '9'

spaceEnglishPunctuation :: T.Text -> T.Text
spaceEnglishPunctuation text =
    foldl spaceOne text englishPunctuation
  where
    spaceOne acc c = T.replace (T.singleton c) (" " <> T.singleton c <> " ") acc

restoreEnglishPunctuation :: T.Text -> T.Text
restoreEnglishPunctuation text =
    restorePunctuation englishOpeningPunctuation englishClosingPunctuation text

restorePunctuation :: [Char] -> [Char] -> T.Text -> T.Text
restorePunctuation opening closing text =
    foldl restoreClosingOne (foldl restoreOpeningOne text opening) closing
  where
    restoreOpeningOne acc c =
        T.replace (p <> " ") p acc
      where
        p = T.singleton c
    restoreClosingOne acc c =
        T.replace (" " <> p) p $
            T.replace (" " <> p <> " ") (p <> " ") acc
      where
        p = T.singleton c

englishPunctuation :: [Char]
englishPunctuation = ".,()–-“:"

englishOpeningPunctuation, englishClosingPunctuation :: [Char]
englishOpeningPunctuation = "(-“"
englishClosingPunctuation = ".,)–”:"

hindiVowels, hindiDiacritics, hindiConsonants :: [Char]
hindiVowels = T.unpack "अआइईउऊऋॠऌॡएऐओऔ"
hindiDiacritics = T.unpack "ािीुूृॄॢॣेैोौ"
hindiConsonants = T.unpack "कखगघङचछजझञटठडढणतथदधनपफबभमयरलवळशषसहफ़ऩड़ढ़ज़"

hindiHalant, hindiNukta :: Char
hindiHalant = '्'
hindiNukta = '़'

hindiHalantText :: T.Text
hindiHalantText = T.singleton hindiHalant

isHindiVocalic :: Char -> Bool
isHindiVocalic c = c `elem` hindiVowels || c `elem` hindiDiacritics

isHindiConsonant :: Char -> Bool
isHindiConsonant c = c `elem` hindiConsonants

charAt :: T.Text -> Int -> Maybe Char
charAt text i
    | i < 0 || i >= T.length text = Nothing
    | otherwise = Just $ T.index text i

lastChar :: T.Text -> Maybe Char
lastChar text
    | T.null text = Nothing
    | otherwise = Just $ T.last text

fixTamilVariants :: T.Text -> T.Text
fixTamilVariants s = T.pack $ go ' ' (T.unpack s)
  where
    go _ [] = []
    go prev [x] = [fix prev x ' ']
    go prev (x : y : rest) = fix prev x y : go x (y : rest)
    fix prev current next
        | current /= 'ந' = current
        | prev == ' ' = 'ந'
        | next == 'த' = 'ந'
        | otherwise = 'ன'

replaceAllLongest :: MappingTable -> T.Text -> T.Text
replaceAllLongest mapping text =
    foldl (\acc (from, to) -> T.replace from to acc) (normalize text) keys
  where
    keys =
        sortOn
            (Down . T.length . fst)
            [(from, to) | (from, to) <- M.toList mapping, not (T.null from)]

shouldSkip :: String -> [(String, String)] -> Bool
shouldSkip name attrs =
    name `elem` ["script", "style"]
        || maybe False (not . null) (lookup "lang" attrs)

outputFor :: PageSpec -> Variant -> FilePath
outputFor page variant
    | variantCode variant == "default" = pageOutput page
    | otherwise = dropExtension (pageOutput page) </> variantCode variant <> ".html"

defaultVariant :: PageSpec -> Variant
defaultVariant page =
    case pageVariants page of
        variant : _ -> variant
        [] -> Variant "default" (pageLanguageLabel page) "en" Nothing
