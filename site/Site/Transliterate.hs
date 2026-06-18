{-# LANGUAGE OverloadedStrings #-}

module Site.Transliterate where

import Data.Char (isAlpha, isSpace)
import Data.List (sortOn)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (Down))
import qualified Data.Text as T
import qualified Data.Text.Normalize as TN
import qualified Data.Yaml as Y
import Control.Applicative ((<|>))
import Site.Config (mappingPathForPage, replacementsFor)
import Site.Types
import System.FilePath (dropExtension, splitDirectories, takeDirectory, (</>))
import Text.HTML.TagSoup hiding (renderTags)
import qualified Text.HTML.TagSoup as TS

type MappingTable = M.Map T.Text T.Text
type MappingSet = M.Map T.Text MappingTable
data TransliterationOverrides = TransliterationOverrides
    { overrideByKey :: M.Map T.Text T.Text
    }

data SanskritBoundary = SanskritBoundary
    { sandhiLeft :: T.Text
    , sandhiRight :: T.Text
    }

data SanskritSutra = SanskritSutra
    { sutraName :: String
    , applySutra :: SanskritBoundary -> Maybe SanskritBoundary
    }

loadTransliterationOverrides :: IO TransliterationOverrides
loadTransliterationOverrides =
    pure $ TransliterationOverrides{overrideByKey = M.empty}

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

transformHtml :: PageSpec -> MappingSet -> TransliterationOverrides -> Variant -> String -> String
transformHtml page mappings overrides variant body =
    TS.renderTags $ go [] $ parseTags body
  where
    maybeMapping = variantMapping variant >>= (`M.lookup` mappings)
    depthPrefix = concat $ replicate (outputDepth page variant) "../"
    go _ [] = []
    go stack (tag : tags) =
        case tag of
            TagOpen name attrs ->
                let frame = frameFor name attrs
                 in TagOpen name (rewriteAttrs depthPrefix $ removeInternalAttrs attrs) : go (frame : stack) tags
            TagClose _ ->
                tag : go (drop 1 stack) tags
            TagText text
                | skipTransliteration stack -> tag : go stack tags
                | otherwise ->
                    case maybeMapping of
                        Nothing -> tag : go stack tags
                        Just mapping ->
                            let (replacement, stack') = overrideText stack text
                                mappingName = fromMaybe "" $ variantMapping variant
                                output = maybe (transcribeText page mappingName mapping $ T.pack text) id replacement
                             in TagText (T.unpack output) : go stack' tags
            _ -> tag : go stack tags
    frameFor name attrs =
        Frame
            { frameSkip = shouldSkip name attrs
            , frameOverride = transliterationOverride name attrs
            }
    transliterationOverride _ attrs =
        explicitOverride attrs
    explicitOverride attrs = do
        value <- T.pack <$> lookup "data-translit" attrs
        pure $ fromMaybe value $ M.lookup value (overrideByKey overrides)

data TransformFrame = Frame
    { frameSkip :: Bool
    , frameOverride :: Maybe T.Text
    }

skipTransliteration :: [TransformFrame] -> Bool
skipTransliteration = any frameSkip

overrideText :: [TransformFrame] -> String -> (Maybe T.Text, [TransformFrame])
overrideText [] _ = (Nothing, [])
overrideText (frame : rest) text
    | Just replacement <- frameOverride frame =
        if T.all isSpace (T.pack text)
            then (Just $ T.pack text, frame : rest)
            else (Just replacement, frame{frameOverride = Just ""} : rest)
    | otherwise =
        let (replacement, rest') = overrideText rest text
         in (replacement, frame : rest')

outputDepth :: PageSpec -> Variant -> Int
outputDepth page variant =
    length $ filter (not . null) $ splitDirectories $ takeDirectory $ outputFor page variant

rewriteAttrs :: String -> [(String, String)] -> [(String, String)]
rewriteAttrs prefix = fmap rewriteAttr
  where
    rewriteAttr (key, value)
        | key `elem` ["href", "src"] = (key, rewriteUrl prefix value)
        | otherwise = (key, value)

removeInternalAttrs :: [(String, String)] -> [(String, String)]
removeInternalAttrs = filter ((/= "data-translit") . fst)

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
        Just mapping -> T.unpack $ transcribeText page mappingName mapping $ T.pack text

transcribeText :: PageSpec -> T.Text -> MappingTable -> T.Text -> T.Text
transcribeText page mappingName mapping text =
    preserveBoundaryWhitespace text $
        fixTamilVariants
            . applyReplacements page
            . postprocessSource page mappingName
            . transcribeWithoutReplacements mapping
            . preprocessSource page mappingName

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

preprocessSource :: PageSpec -> T.Text -> T.Text -> T.Text
preprocessSource page mappingName
    | isHindiPage page && mappingName == "to_devanagari" = insertHindiOrthographicSchwas . normalizeHindiNasalization
    | isHindiPage page = normalizeHindiNasalization
    | isTamilPage page = normalizeTamilSource
    | isSanskritPage page = applySanskritSandhi . normalizeSanskritSource
    | otherwise = id

postprocessSource :: PageSpec -> T.Text -> T.Text -> T.Text
postprocessSource page mappingName
    | isHindiPage page && mappingName == "to_english" = shortenFinalHindiEnglishVowels
    | otherwise = cleanupCommonPostprocess

isHindiPage :: PageSpec -> Bool
isHindiPage page = pageSource page == "content/hindi.md" || ".hi.md" `endsWith` pageSource page

isTamilPage :: PageSpec -> Bool
isTamilPage page = pageSource page == "content/tamil.md" || ".ta.md" `endsWith` pageSource page

isSanskritPage :: PageSpec -> Bool
isSanskritPage page = pageSource page == "content/sanskrit.md" || ".sa.md" `endsWith` pageSource page

cleanupCommonPostprocess :: T.Text -> T.Text
cleanupCommonPostprocess =
    collapseDigitPeriodSpaces . T.replace " -" "-" . T.replace "- " "-"

insertHindiOrthographicSchwas :: T.Text -> T.Text
insertHindiOrthographicSchwas = T.concat . go . tokenizeIndicLatin
  where
    go [] = []
    go [tok]
        | isLatinConsonant tok = [tok, "a"]
        | otherwise = [tok]
    go (tok : next : rest)
        | isLatinConsonant tok && not (isLatinVowel next || isLatinModifier next) && not (continuesWord next) =
            tok : "a" : go (next : rest)
        | isLatinConsonant tok && not (isLatinVowel next || isLatinModifier next) && isWordBoundary next =
            tok : "a" : go (next : rest)
        | otherwise = tok : go (next : rest)

normalizeHindiNasalization :: T.Text -> T.Text
normalizeHindiNasalization = T.concat . fmap normalizeHindiToken . hindiTokens

normalizeHindiToken :: T.Text -> T.Text
normalizeHindiToken token
    | not $ T.any isHindiLatinWordChar token = token
    | otherwise =
        replaceMany
            [ ("ain", "aiṁ")
            , ("āng", "āṁg")
            , ("ank", "aṁk")
            , ("ing", "iṁg")
            , ("īnc", "īṁc")
            , ("tantr", "taṁtr")
            , ("sanskr̥t", "saṁskr̥t")
            , ("pasand", "pasaṁd")
            , ("menṭ", "meṁṭ")
            , ("eng", "eṁg")
            ]
            $ normalizeHindiNasalSuffix
            $ fromMaybe token
            $ M.lookup token hindiNasalWords

hindiNasalWords :: M.Map T.Text T.Text
hindiNasalWords =
    M.fromList
        [ ("hūn", "hūm̐")
        , ("main", "maiṁ")
        , ("mainne", "maiṁne")
        , ("hain", "haiṁ")
        , ("men", "meṁ")
        , ("nahīn", "nahīṁ")
        ]

normalizeHindiNasalSuffix :: T.Text -> T.Text
normalizeHindiNasalSuffix token =
    fromMaybe token $
        firstJust
            [ replaceSuffix "ron" "roṁ" token
            , replaceSuffix "yon" "yoṁ" token
            , replaceSuffix "gon" "goṁ" token
            , replaceSuffix "tron" "troṁ" token
            , replaceSuffix "ṭon" "ṭoṁ" token
            , replaceSuffix "āon" "āoṁ" token
            , replaceSuffix "āen" "āeṁ" token
            , replaceSuffix "īyān" "īyām̐" token
            , replaceSuffix "īren" "īreṁ" token
            ]

replaceSuffix :: T.Text -> T.Text -> T.Text -> Maybe T.Text
replaceSuffix suffix replacement token =
    (<> replacement) <$> T.stripSuffix suffix token

normalizeTamilSource :: T.Text -> T.Text
normalizeTamilSource = T.concat . fmap normalizeTamilToken . tamilTokens

normalizeTamilToken :: T.Text -> T.Text
normalizeTamilToken token
    | not $ T.any isTamilLatinWordChar token = token
    | otherwise =
        T.replace tamilDentalNMarker "n" $
            inferTamilAlveolarN $
                assimilateTamilNasals $
                replaceMany
                    [ ("nh", tamilDentalNMarker)
                    , ("ndr", "ṉṟ")
                    , ("ksh", "kṣ")
                    , ("zh", "ḻ")
                    , ("rr", "ṟṟ")
                    , ("rh", "ṟ")
                    , ("sh", "ṣ")
                    , ("g", "k")
                    , ("ḍ", "ṭ")
                    ]
                    token

tamilDentalNMarker :: T.Text
tamilDentalNMarker = "\xf0000"

assimilateTamilNasals :: T.Text -> T.Text
assimilateTamilNasals = T.pack . go . T.unpack
  where
    go [] = []
    go [c] = [c]
    go (c : next : rest)
        | c == 'n' = nasalBefore next : go (next : rest)
        | otherwise = c : go (next : rest)
    nasalBefore next =
        case next of
            'k' -> 'ṅ'
            'c' -> 'ñ'
            'j' -> 'ñ'
            'ṭ' -> 'ṇ'
            't' -> 'n'
            'p' -> 'm'
            _ -> 'n'

inferTamilAlveolarN :: T.Text -> T.Text
inferTamilAlveolarN = T.pack . go True . T.unpack
  where
    go _ [] = []
    go atStart [c] = [rewrite atStart c Nothing]
    go atStart (c : next : rest) = rewrite atStart c (Just next) : go False (next : rest)
    rewrite atStart c next
        | c /= 'n' = c
        | atStart = 'n'
        | next == Just 't' = 'n'
        | otherwise = 'ṉ'

tamilTokens :: T.Text -> [T.Text]
tamilTokens text
    | T.null text = []
    | isTamilLatinWordChar (T.head text) =
        let (word, rest) = T.span isTamilLatinWordChar text
         in word : tamilTokens rest
    | otherwise =
        let (other, rest) = T.span (not . isTamilLatinWordChar) text
         in other : tamilTokens rest

isTamilLatinWordChar :: Char -> Bool
isTamilLatinWordChar c =
    isAlpha c || c `elem` ("̥̄̐̃" :: String)

hindiTokens :: T.Text -> [T.Text]
hindiTokens text
    | T.null text = []
    | isHindiLatinWordChar (T.head text) =
        let (word, rest) = T.span isHindiLatinWordChar text
         in word : hindiTokens rest
    | otherwise =
        let (other, rest) = T.span (not . isHindiLatinWordChar) text
         in other : hindiTokens rest

isHindiLatinWordChar :: Char -> Bool
isHindiLatinWordChar c =
    isAlpha c || c `elem` ("̥̄̐̃" :: String)

replaceMany :: [(T.Text, T.Text)] -> T.Text -> T.Text
replaceMany replacements text =
    foldl (\acc (from, to) -> T.replace from to acc) text replacements

normalizeSanskritSource :: T.Text -> T.Text
normalizeSanskritSource =
    replaceMany
        [ ("sh", "ṣ")
        ]

applySanskritSandhi :: T.Text -> T.Text
applySanskritSandhi =
    T.unwords . filter (not . T.null) . sandhiWords . T.words
  where
    sandhiWords (a : b : rest) =
        let (a', b') = sandhiPair a b
         in if T.null b'
                then sandhiWords (a' : rest)
                else a' : sandhiWords (b' : rest)
    sandhiWords [word] = [finalizeSanskritWord word]
    sandhiWords words' = words'

sandhiPair :: T.Text -> T.Text -> (T.Text, T.Text)
sandhiPair left right =
    fromMaybe (finalizeSanskritWord left, right) $ do
        (leftCore, leftSuffix) <- splitTrailingSanskritPunctuation left
        (rightPrefix, rightCore) <- splitLeadingSanskritPunctuation right
        boundary <- applySanskritSutras $ SanskritBoundary leftCore rightCore
        let left' = sandhiLeft boundary
            right' = sandhiRight boundary
        pure (left' <> leftSuffix, rightPrefix <> right')

applySanskritSutras :: SanskritBoundary -> Maybe SanskritBoundary
applySanskritSutras boundary =
    firstJust [applySutra sutra boundary | sutra <- sanskritSutras]

sanskritSutras :: [SanskritSutra]
sanskritSutras =
    [ SanskritSutra "8.3.23 mo 'nusvāraḥ" sutraMoNusvarah
    , SanskritSutra "6.1.101 akaḥ savarṇe dīrghaḥ" sutraAkahSavarnaDirghah
    , SanskritSutra "6.1.87 ādguṇaḥ" sutraAdGunah
    , SanskritSutra "6.1.88 vṛddhir eci" sutraVrddhirEci
    , SanskritSutra "6.1.77 iko yaṇ aci" sutraIkoYanAci
    , SanskritSutra "6.1.78 eco 'yavāyāvaḥ" sutraEcoYavayavah
    , SanskritSutra "8.2.66 sasajuṣo ruḥ" sutraSasajusoRuh
    , SanskritSutra "8.3.15 kharavasānayor visarjanīyaḥ" sutraKharavasanayorVisarjaniyah
    , SanskritSutra "8.4.40 stoḥ ścunā ścuḥ" sutraStohScunaScuh
    , SanskritSutra "8.4.41 ṣṭunā ṣṭuḥ" sutraStunaStuh
    , SanskritSutra "8.4.55 khari ca" sutraKhariCa
    ]

sutraMoNusvarah :: SanskritBoundary -> Maybe SanskritBoundary
sutraMoNusvarah boundary = do
    (stem, final) <- unsnocText $ sandhiLeft boundary
    (initial, _) <- unconsText $ sandhiRight boundary
    if final == 'm' && isSanskritConsonant initial
        then Just boundary{sandhiLeft = stem <> "ṃ"}
        else Nothing

sutraAkahSavarnaDirghah :: SanskritBoundary -> Maybe SanskritBoundary
sutraAkahSavarnaDirghah = rewriteJoinedVowelBoundary $ \final initial ->
    case (final, initial) of
        ('a', 'a') -> Just ("ā", "")
        ('a', 'ā') -> Just ("ā", "")
        ('ā', 'a') -> Just ("ā", "")
        ('ā', 'ā') -> Just ("ā", "")
        ('i', 'i') -> Just ("ī", "")
        ('i', 'ī') -> Just ("ī", "")
        ('ī', 'i') -> Just ("ī", "")
        ('ī', 'ī') -> Just ("ī", "")
        ('u', 'u') -> Just ("ū", "")
        ('u', 'ū') -> Just ("ū", "")
        ('ū', 'u') -> Just ("ū", "")
        ('ū', 'ū') -> Just ("ū", "")
        ('ṛ', 'ṛ') -> Just ("ṝ", "")
        _ -> Nothing

sutraAdGunah :: SanskritBoundary -> Maybe SanskritBoundary
sutraAdGunah = rewriteJoinedVowelBoundary $ \final initial ->
    case (final, initial) of
        ('a', 'i') -> Just ("e", "")
        ('a', 'ī') -> Just ("e", "")
        ('ā', 'i') -> Just ("e", "")
        ('ā', 'ī') -> Just ("e", "")
        ('a', 'u') -> Just ("o", "")
        ('a', 'ū') -> Just ("o", "")
        ('ā', 'u') -> Just ("o", "")
        ('ā', 'ū') -> Just ("o", "")
        ('a', 'ṛ') -> Just ("ar", "")
        ('ā', 'ṛ') -> Just ("ar", "")
        ('a', 'ṝ') -> Just ("ar", "")
        ('ā', 'ṝ') -> Just ("ar", "")
        ('a', 'ḷ') -> Just ("al", "")
        ('ā', 'ḷ') -> Just ("al", "")
        _ -> Nothing

sutraVrddhirEci :: SanskritBoundary -> Maybe SanskritBoundary
sutraVrddhirEci = rewriteJoinedVowelBoundary $ \final initial ->
    case (final, initial) of
        ('a', 'e') -> Just ("ai", "")
        ('ā', 'e') -> Just ("ai", "")
        ('a', 'o') -> Just ("au", "")
        ('ā', 'o') -> Just ("au", "")
        _ -> Nothing

sutraIkoYanAci :: SanskritBoundary -> Maybe SanskritBoundary
sutraIkoYanAci = rewriteVowelBoundary $ \final initial ->
    if isSanskritVowel initial
        then case final of
            'i' -> Just ("y", T.singleton initial)
            'ī' -> Just ("y", T.singleton initial)
            'u' -> Just ("v", T.singleton initial)
            'ū' -> Just ("v", T.singleton initial)
            'ṛ' -> Just ("r", T.singleton initial)
            'ṝ' -> Just ("r", T.singleton initial)
            'ḷ' -> Just ("l", T.singleton initial)
            _ -> Nothing
        else Nothing

sutraEcoYavayavah :: SanskritBoundary -> Maybe SanskritBoundary
sutraEcoYavayavah boundary
    | Just stem <- T.stripSuffix "ai" (sandhiLeft boundary), Just (initial, _) <- unconsText (sandhiRight boundary), isSanskritVowel initial =
        Just boundary{sandhiLeft = stem <> "āy" <> sandhiRight boundary, sandhiRight = ""}
    | Just stem <- T.stripSuffix "au" (sandhiLeft boundary), Just (initial, _) <- unconsText (sandhiRight boundary), isSanskritVowel initial =
        Just boundary{sandhiLeft = stem <> "āv" <> sandhiRight boundary, sandhiRight = ""}
    | otherwise =
        rewriteVowelBoundary eco boundary
  where
    eco 'e' 'a' = Just ("e", "’")
    eco 'o' 'a' = Just ("o", "’")
    eco 'e' initial | isSanskritVowel initial = Just ("ay", T.singleton initial)
    eco 'o' initial | isSanskritVowel initial = Just ("av", T.singleton initial)
    eco _ _ = Nothing

sutraSasajusoRuh :: SanskritBoundary -> Maybe SanskritBoundary
sutraSasajusoRuh boundary = do
    stem <- T.stripSuffix "s" $ sandhiLeft boundary
    (initial, _) <- unconsText $ sandhiRight boundary
    if isSanskritVowel initial || isSanskritVoiced initial
        then
            let (left', right') = ruhOutcome stem (sandhiRight boundary)
             in Just boundary{sandhiLeft = left', sandhiRight = right'}
        else Nothing

sutraKharavasanayorVisarjaniyah :: SanskritBoundary -> Maybe SanskritBoundary
sutraKharavasanayorVisarjaniyah boundary = do
    stem <- T.stripSuffix "s" $ sandhiLeft boundary
    (initial, _) <- unconsText $ sandhiRight boundary
    if isSanskritVoiceless initial
        then Just boundary{sandhiLeft = stem <> "ḥ"}
        else Nothing

sutraStohScunaScuh :: SanskritBoundary -> Maybe SanskritBoundary
sutraStohScunaScuh = rewriteFinalBeforeInitial $ \final initial ->
    if final == 't' && isSanskritPalatal initial then Just "c" else Nothing

sutraStunaStuh :: SanskritBoundary -> Maybe SanskritBoundary
sutraStunaStuh = rewriteFinalBeforeInitial $ \final initial ->
    if final == 't' && isSanskritRetroflex initial then Just "ṭ" else Nothing

sutraKhariCa :: SanskritBoundary -> Maybe SanskritBoundary
sutraKhariCa = rewriteFinalBeforeInitial $ \final initial ->
    case final of
        'd' | isSanskritVoiceless initial -> Just "t"
        'r' | isSanskritVoiceless initial -> Just "ḥ"
        't' | isSanskritVoiced initial -> Just "d"
        'n' | isSanskritLabial initial -> Just "m"
        _ -> Nothing

rewriteVowelBoundary :: (Char -> Char -> Maybe (T.Text, T.Text)) -> SanskritBoundary -> Maybe SanskritBoundary
rewriteVowelBoundary rewrite boundary = do
    (stem, final) <- unsnocText $ sandhiLeft boundary
    (initial, rest) <- unconsText $ sandhiRight boundary
    (leftEnding, rightBeginning) <- rewrite final initial
    pure boundary{sandhiLeft = stem <> leftEnding, sandhiRight = rightBeginning <> rest}

rewriteJoinedVowelBoundary :: (Char -> Char -> Maybe (T.Text, T.Text)) -> SanskritBoundary -> Maybe SanskritBoundary
rewriteJoinedVowelBoundary rewrite boundary = do
    (stem, final) <- unsnocText $ sandhiLeft boundary
    (initial, rest) <- unconsText $ sandhiRight boundary
    (leftEnding, rightBeginning) <- rewrite final initial
    pure boundary{sandhiLeft = stem <> leftEnding <> rightBeginning <> rest, sandhiRight = ""}

rewriteFinalBeforeInitial :: (Char -> Char -> Maybe T.Text) -> SanskritBoundary -> Maybe SanskritBoundary
rewriteFinalBeforeInitial rewrite boundary = do
    (stem, final) <- unsnocText $ sandhiLeft boundary
    (initial, _) <- unconsText $ sandhiRight boundary
    final' <- rewrite final initial
    pure boundary{sandhiLeft = stem <> final'}

ruhOutcome :: T.Text -> T.Text -> (T.Text, T.Text)
ruhOutcome stem right =
    case unsnocText stem of
        Just (before, 'a')
            | T.isPrefixOf "a" right -> (before <> "o", "’" <> T.drop 1 right)
            | otherwise -> (before <> "o", right)
        Just (_, 'ā') -> (stem, right)
        Just (_, vowel)
            | vowel `elem` ("iīuū" :: String) -> (stem <> "r", right)
        _ -> (stem <> "ḥ", right)

firstJust :: [Maybe a] -> Maybe a
firstJust = foldr (<|>) Nothing

finalizeSanskritWord :: T.Text -> T.Text
finalizeSanskritWord word =
    case splitTrailingSanskritPunctuation word of
        Just (core, suffix)
            | T.isSuffixOf "s" core -> T.dropEnd 1 core <> "ḥ" <> suffix
        _ -> word

splitTrailingSanskritPunctuation :: T.Text -> Maybe (T.Text, T.Text)
splitTrailingSanskritPunctuation text =
    let (suffix, core) = T.span isSanskritPunctuation $ T.reverse text
     in if T.null core then Nothing else Just (T.reverse core, T.reverse suffix)

splitLeadingSanskritPunctuation :: T.Text -> Maybe (T.Text, T.Text)
splitLeadingSanskritPunctuation text =
    let (prefix, core) = T.span isSanskritPunctuation text
     in if T.null core then Nothing else Just (prefix, core)

isSanskritVowel, isSanskritConsonant, isSanskritVoiced, isSanskritVoiceless, isSanskritPalatal, isSanskritRetroflex, isSanskritLabial, isSanskritPunctuation :: Char -> Bool
isSanskritVowel c = c `elem` ("aāiīuūṛṝḷḹeo" :: String)
isSanskritConsonant c = c `elem` ("kKgGṅcCjJñṭṬḍḌṇtTdDnpPbBmyrlvśṣshfṉ" :: String)
isSanskritVoiced c = isSanskritVowel c || c `elem` ("gGṅjJñḍḌṇdDnbBmyrlvh" :: String)
isSanskritVoiceless c = c `elem` ("kKcCṭṬtTpPśṣs" :: String)
isSanskritPalatal c = c `elem` ("cCjJñś" :: String)
isSanskritRetroflex c = c `elem` ("ṭṬḍḌṇṣ" :: String)
isSanskritLabial c = c `elem` ("pPbBmf" :: String)
isSanskritPunctuation c = c `elem` (".,;:!?)]}।-" :: String)

tokenizeIndicLatin :: T.Text -> [T.Text]
tokenizeIndicLatin text
    | T.null text = []
    | otherwise =
        case longestPrefix indicLatinTokens text of
            Just token -> token : tokenizeIndicLatin (T.drop (T.length token) text)
            Nothing -> T.singleton (T.head text) : tokenizeIndicLatin (T.tail text)

longestPrefix :: [T.Text] -> T.Text -> Maybe T.Text
longestPrefix tokens text =
    listToMaybe $ sortOn (Down . T.length) $ filter (`T.isPrefixOf` text) tokens

indicLatinTokens :: [T.Text]
indicLatinTokens = latinConsonants <> latinVowels <> latinModifiers

latinVowels, latinModifiers, latinConsonants :: [T.Text]
latinVowels = ["ai", "au", "r̥̄", "l̥̄", "r̥", "l̥", "ā", "ī", "ū", "ṛ", "ṝ", "ḷ", "ḹ", "a", "i", "u", "e", "o"]
latinModifiers = ["ṃ", "ṁ", "m̐", "̃", "ḥ"]
latinConsonants =
    [ "kṣ", "jñ", "kh", "gh", "ch", "jh", "ṭh", "ḍh", "th", "dh", "ph", "bh", "ṛh"
    , "k", "g", "ṅ", "c", "j", "ñ", "ṭ", "ḍ", "ṇ", "t", "d", "n", "p", "b", "m"
    , "y", "r", "l", "v", "ḻ", "ḷ", "ś", "ṣ", "s", "h", "f", "ṉ", "ṛ", "z"
    ]

isLatinVowel, isLatinModifier, isLatinConsonant, continuesWord, isWordBoundary :: T.Text -> Bool
isLatinVowel tok = tok `elem` latinVowels
isLatinModifier tok = tok `elem` latinModifiers
isLatinConsonant tok = tok `elem` latinConsonants
continuesWord tok =
    isLatinVowel tok || isLatinModifier tok || isLatinConsonant tok || T.all (`elem` ("'" :: String)) tok
isWordBoundary tok = T.all (\c -> isSpace c || c `elem` (".,;:!?)]}।-" :: String)) tok

unsnocText :: T.Text -> Maybe (T.Text, Char)
unsnocText text
    | T.null text = Nothing
    | otherwise = Just (T.init text, T.last text)

unconsText :: T.Text -> Maybe (Char, T.Text)
unconsText text
    | T.null text = Nothing
    | otherwise = Just (T.head text, T.tail text)

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
    collapseDigitPeriodSpaces . T.replace " -" "-" . T.replace "- " "-"

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
    go prev (x : rest) = fix prev x (nextBase rest) : go x rest
    nextBase ('்' : x : _) = x
    nextBase (x : _) = x
    nextBase [] = ' '
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
