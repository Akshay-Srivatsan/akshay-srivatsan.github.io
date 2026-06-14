{-# LANGUAGE OverloadedStrings #-}

module Site.Config where

import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Site.Types
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (dropExtension, (</>))

baseUrl :: String
baseUrl = "https://aks.io"

authorName :: String
authorName = "Akshay Srivatsan"

currentYear :: String
currentYear = "2026"

fixedPages :: [PageSpec]
fixedPages =
  [ PageSpec "content/index.md" "index.html" "home" "English" [englishVariant],
    PageSpec "content/latin.md" "latin.html" "home" "Latin" latinVariants,
    PageSpec "content/tamil.md" "tamil.html" "home" "Tamil" tamilVariants,
    PageSpec "content/sanskrit.md" "sanskrit.html" "home" "Sanskrit" sanskritVariants,
    PageSpec "content/hindi.md" "hindi.html" "home" "Hindi" hindiVariants,
    PageSpec "content/courses.md" "courses.html" "courses" "English" [englishVariant],
    PageSpec "content/jobs.md" "jobs.html" "jobs" "English" [englishVariant],
    PageSpec "content/shows.md" "shows.html" "shows" "English" [englishVariant]
  ]

englishVariant :: Variant
englishVariant = Variant "default" "English" "en" Nothing

latinVariants, tamilVariants, sanskritVariants, hindiVariants :: [Variant]
latinVariants =
  [ Variant "default" "quadratae" "la" Nothing,
    Variant "unciali" "unciales" "la-Latg" (Just "to_ascii"),
    Variant "italica" "italicae" "la-Ital" (Just "to_italics"),
    Variant "tamulica" "tamulicae" "ta" (Just "to_tamil"),
    Variant "grantha" "granthae" "ta" (Just "to_grantha"),
    Variant "brahmi" "brahmes" "ta" (Just "to_brahmi")
  ]
tamilVariants =
  [ Variant "default" "தமிழ்" "ta" Nothing,
    Variant "brahmi" "பிராமி" "ta-Brah" (Just "to_brahmi"),
    Variant "devanagari" "தேவநாகரி" "ta-Deva" (Just "to_devanagari"),
    Variant "iso" "லத்தீன்" "ta-Latn" (Just "to_iso"),
    Variant "ipa" "சர்வதேச" "ta-phonipa" (Just "to_ipa"),
    Variant "aangilam" "ஆங்கிலம்" "ta-Latn" (Just "to_english")
  ]
sanskritVariants =
  [ Variant "default" "lātin" "sa-Latn" Nothing,
    Variant "devanagari" "devanāgarī" "sa" (Just "to_devanagari"),
    Variant "tamil" "tamiḻ" "sa-Taml" (Just "to_tamil"),
    Variant "tamil-grantha" "tamiḻ-grantha" "sa-Xaaa" (Just "to_tamil_grantha"),
    Variant "grantha" "grantha" "sa-Gran" (Just "to_grantha"),
    Variant "brahmi" "brāhmī" "sa-Brah" (Just "to_brahmi"),
    Variant "ipa" "sarvadeśīya" "sa-phonipa" (Just "to_ipa"),
    Variant "anglabhasha" "āṅglabhāśā" "sa-Latn" (Just "to_english"),
    Variant "simple" "sarala" "sa-Latn" (Just "to_simple")
  ]
hindiVariants =
  [ Variant "default" "देवनागरी" "hi" Nothing,
    Variant "tamil" "तमिल" "hi-Taml" (Just "to_tamil"),
    Variant "iso" "लातिन" "hi-Latn" (Just "to_iso"),
    Variant "ipa" "ध्वन्यात्मक" "hi-phonipa" (Just "to_ipa"),
    Variant "angrezi" "आंग्रेज़ी" "hi-Latn" (Just "to_english")
  ]

discoverBlogPages :: IO [PageSpec]
discoverBlogPages = do
  let dir = "content/blog"
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else mapMaybe (blogSpec dir) <$> listDirectory dir

blogSpec :: FilePath -> FilePath -> Maybe PageSpec
blogSpec dir file = do
  guardSuffix ".md" file
  let base = dropExtension file
      (slug, langPart) = splitLang base
      source = dir </> file
      groupKey = "blog:" <> slug
      (label, variants) = languageConfig langPart
      output = case langPart of
        "en" -> "blog" </> slug <> ".html"
        lang -> lang </> "blog" </> slug <> ".html"
  pure $ PageSpec source output groupKey label variants
  where
    guardSuffix suffix x = if suffix `isSuffixOf` x then Just () else Nothing
    splitLang x =
      let (name, ext) = splitFileNameByLastDot x
       in if null ext then (x, "en") else (name, ext)
    splitFileNameByLastDot x =
      let rev = reverse x
       in case break (== '.') rev of
            (_, "") -> (x, "")
            (ext, _ : rest) -> (reverse rest, reverse ext)

languageConfig :: String -> (String, [Variant])
languageConfig "ta" = ("Tamil", tamilVariants)
languageConfig "sa" = ("Sanskrit", sanskritVariants)
languageConfig "la" = ("Latin", latinVariants)
languageConfig "hi" = ("Hindi", hindiVariants)
languageConfig _ = ("English", [englishVariant])

mappingPathForPage :: PageSpec -> Maybe FilePath
mappingPathForPage page
  | pageSource page == "content/latin.md" || ".la.md" `isSuffixOf` pageSource page = Just "transliterate/latin.yaml"
  | pageSource page == "content/tamil.md" || ".ta.md" `isSuffixOf` pageSource page = Just "transliterate/tamil.yaml"
  | pageSource page == "content/sanskrit.md" || ".sa.md" `isSuffixOf` pageSource page = Just "transliterate/sanskrit.yaml"
  | pageSource page == "content/hindi.md" || ".hi.md" `isSuffixOf` pageSource page = Just "transliterate/hindi.yaml"
  | otherwise = Nothing

isBlogPage :: PageSpec -> Bool
isBlogPage page = "content/blog/" `isPrefixOf` pageSource page

isLatinPage :: PageSpec -> Bool
isLatinPage page = pageSource page == "content/latin.md" || ".la.md" `isSuffixOf` pageSource page

replacementsFor :: PageSpec -> [(T.Text, T.Text)]
replacementsFor page
  | pageSource page == "content/tamil.md" =
      [ ("akshay", "Akshay"),
        ("sreevadhsan", "Srivatsan"),
        ("ɕɾiːʋadsan", "ɕɾiːʋatsan"),
        ("श्रीवत्सऩ्", "श्रीवत्सन्"),
        ("kanini", "ganini"),
        ("kaɳini", "gaɳini"),
        ("sdaanford", "Stanford"),
        ("sdaanbord", "Stanford"),
        ("menlo", "Menlo"),
        ("yunigs", "UNIX")
      ]
  | pageSource page == "content/sanskrit.md" =
      [ ("akshay", "Akshay"),
        ("shreevatsan", "Srivatsan"),
        ("sṭenforḍ", "Stanford")
      ]
  | pageSource page == "content/hindi.md" =
      [ ("akshay", "Akshay"),
        ("shreevatsan", "Srivatsan"),
        ("stainford", "Stanford"),
        ("yooniversiti", "University"),
        ("தமில்", "தமிழ்")
      ]
  | otherwise = []
