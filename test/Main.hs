{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import Data.Map.Strict qualified as M
import Site.Config
import Site.Transliterate
import Site.Types
import System.Exit (exitFailure)
import Text.Pandoc.Definition

main :: IO ()
main = do
  routeTests
  localeTests
  transliterationTests
  putStrLn "site tests passed"

routeTests :: IO ()
routeTests = do
  assertEqual "root default" "index.html" $ routeFor "" Default
  assertEqual "root tagged" "ta-Brah.html" $ routeFor "" (LanguageTag "ta-Brah")
  assertEqual "page default" "courses/index.html" $ routeFor "courses" Default
  assertEqual "page tagged" "courses/sa.html" $ routeFor "courses" (LanguageTag "sa")
  assertEqual "post default" "2026/07/21/example/index.html" $ routeFor "2026/07/21/example" Default
  assertEqual "post tagged" "2026/07/21/example/ta.html" $ routeFor "2026/07/21/example" (LanguageTag "ta")
  assertEqual "default URL" "/courses/" $ urlFor "courses" Default
  assertEqual "tagged URL" "/courses/ta.html" $ urlFor "courses" (LanguageTag "ta")

localeTests :: IO ()
localeTests = do
  locales <- loadLocales "locales.yaml"
  tamil <- require "Tamil locale" $ M.lookup "ta" locales
  assertEqual "Tamil language name" "tamiḻ" $ localeName tamil
  assertEqual "Tamil script label" (Just "pirāmi") $ M.lookup "ta-Brah" $ localeScripts tamil
  assertEqual "Tamil selector heading" (Just "kiṭaikkum moḻikaḷ") $ M.lookup "available-languages" $ localeStrings tamil

transliterationTests :: IO ()
transliterationTests = do
  mappings <- loadMappings "transliterate/ta.yaml"
  assertEqual
    "Tamil tags"
    ["ta-Latn-x-simple", "ta", "ta-Brah", "ta-Latn"]
    (mappingTags mappings)
  tamil <- require "Tamil mapping" $ mappingFor mappings "ta"
  let document = Pandoc nullMeta [Para [Str "tamiḻ", Space, Code nullAttr "code", Space, Span ("", [], [("lang", "en")]) [Str "English"]]]
      transformed = transliteratePandoc "ta" "" tamil document
  case transformed of
    Pandoc _ [Para (Str word : _)] -> assertEqual "Tamil prose transformed" "தமிழ்" word
    _ -> failTest "unexpected Pandoc shape"
  let protected = Pandoc nullMeta [Para [Span ("", [], [("lang", "en")]) [Str "English"], Space, Code nullAttr "code"]]
  assertEqual "protected Pandoc nodes" protected $ transliteratePandoc "ta" "" tamil protected
  let overridden = Pandoc nullMeta [Para [Span ("", [], [("data-translit", "literal")]) [Str "original"]]]
      expectedOverride = Pandoc nullMeta [Para [Span nullAttr [Str "literal"]]]
      expectedIdentity = Pandoc nullMeta [Para [Span nullAttr [Str "original"]]]
  assertEqual "explicit transliteration override" expectedOverride $ transliteratePandoc "ta" "" tamil overridden
  assertEqual "identity removes internal attributes" expectedIdentity $ transliteratePandoc "ta" "" Identity overridden

require :: String -> Maybe a -> IO a
require _ (Just value) = pure value
require label Nothing = failTest $ label <> " was missing"

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  unless (expected == actual) $
    failTest $ label <> ": expected " <> show expected <> ", got " <> show actual

failTest :: String -> IO a
failTest message = do
  putStrLn $ "FAIL: " <> message
  exitFailure
