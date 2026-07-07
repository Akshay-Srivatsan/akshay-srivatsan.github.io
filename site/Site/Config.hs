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
    PageSpec "content/tamil.md" "tamil.html" "home" "Tamil" tamilVariants,
    PageSpec "content/latin.md" "latin.html" "home" "Latin" [latinVariant],
    PageSpec "content/sanskrit.md" "sanskrit.html" "home" "Sanskrit" sanskritVariants,
    PageSpec "content/courses.md" "courses.html" "courses" "English" [englishVariant],
    PageSpec "content/jobs.md" "jobs.html" "jobs" "English" [englishVariant],
    PageSpec "content/shows.md" "shows.html" "shows" "English" [englishVariant]
  ]

englishVariant :: Variant
englishVariant = Variant "default" "English" "en" Nothing

latinVariant :: Variant
latinVariant = Variant "default" "Latin" "la" Nothing

tamilVariants, sanskritVariants :: [Variant]
tamilVariants =
  [ Variant "default" "தமிழ்" "ta" (Just "to_tamil"),
    Variant "iso" "லத்தீன்" "ta-Latn" (Just "to_iso"),
    Variant "simple" "எளியது" "ta-Latn" Nothing,
    Variant "brahmi" "பிராமி" "ta-Brah" (Just "to_brahmi")
  ]
sanskritVariants =
  [ Variant "default" "devanāgarī" "sa" (Just "to_devanagari"),
    Variant "iast" "lātin" "sa-Latn" (Just "to_iast"),
    Variant "simple" "sarala" "sa-Latn" Nothing,
    Variant "grantha" "grantha" "sa-Gran" (Just "to_grantha"),
    Variant "tamil-grantha" "tamiḻ-grantha" "sa-Xaaa" (Just "to_tamil_grantha"),
    Variant "brahmi" "brāhmī" "sa-Brah" (Just "to_brahmi")
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
languageConfig "la" = ("Latin", [latinVariant])
languageConfig "en" = ("English", [englishVariant])
languageConfig _ = error "unsupported language"

mappingPathForPage :: PageSpec -> Maybe FilePath
mappingPathForPage page
  | pageSource page == "content/tamil.md" || ".ta.md" `isSuffixOf` pageSource page = Just "transliterate/tamil.yaml"
  | pageSource page == "content/sanskrit.md" || ".sa.md" `isSuffixOf` pageSource page = Just "transliterate/sanskrit.yaml"
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
  | otherwise = []
