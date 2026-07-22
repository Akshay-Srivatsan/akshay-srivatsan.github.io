{-# LANGUAGE OverloadedStrings #-}

module Site.DateScript where

import Data.List (intercalate)
import Site.Types

scriptsFor :: FilePath -> OutputLanguage -> (String -> String) -> String
scriptsFor logicalPath language transform
    | logicalPath == "" && primaryLanguage (requestedTag language) == "la" =
        "<script src=\"/assets/js/date.js\" defer"
            <> concatMap dateListAttr dateLists
            <> concatMap dateTextAttr dateTexts
            <> "></script>"
    | otherwise = ""
  where
    dateListAttr (name, values) =
        " data-" <> name <> "=\"" <> escapeHtmlAttr (intercalate "\t" $ fmap transform values) <> "\""
    dateTextAttr (name, value) =
        " data-" <> name <> "=\"" <> escapeHtmlAttr (transform value) <> "\""

dateLists :: [(String, [String])]
dateLists =
    [ ( "months-accusative"
      ,
        [ ""
        , "Jānuāriās"
        , "Februāriās"
        , "Mārtiās"
        , "Aprīlēs"
        , "Maiās"
        , "Jūniās"
        , "Jūlia"
        , "Augustās"
        , "Septembrēs"
        , "Octōbrēs"
        , "Novembrēs"
        , "Decembrēs"
        , "Jānuāriās"
        ]
      )
    , ( "months-ablative"
      ,
        [ ""
        , "Jānuāriīs"
        , "Februāriīs"
        , "Mārtiīs"
        , "Aprīlēs"
        , "Maiīs"
        , "Jūniīs"
        , "Jūlia"
        , "Augustīs"
        , "Septembrēs"
        , "Octōbrēs"
        , "Novembrēs"
        , "Decembrēs"
        , "Jānuāriīs"
        ]
      )
    , ( "numbers-accusative"
      ,
        [ "nihil"
        , "prīmum"
        , "secundum"
        , "tertium"
        , "quārtum"
        , "quīntum"
        , "sextum"
        , "septimum"
        , "octāvum"
        , "nōnum"
        , "decimum"
        , "ūndecimum"
        , "duodecimum"
        , "tertium decimum"
        , "quārtum decimum"
        , "quīntum decimum"
        , "sextum decimum"
        , "septimum decimum"
        , "duodēvicēsimus"
        , "ūndēvīcēsimus"
        , "vīcēsimus"
        ]
      )
    , ( "months-short"
      , ["", "Iān.", "Feb.", "Mār.", "Apr.", "Mai.", "Iūn.", "Iūl.", "Aug.", "Sept.", "Oct.", "Nov.", "Dec.", "Iān."]
      )
    , ( "numbers-short"
      , ["N", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", "XVIII", "XIX", "XX"]
      )
    ]

dateTexts :: [(String, String)]
dateTexts =
    [ ("date-label", "hodie est")
    , ("ante-diem", "ante diem")
    , ("nonas", "nōnās")
    , ("nonis", "nōnīs")
    , ("nonas-singular", "nōnas")
    , ("idus", "īdūs")
    , ("idibus", "īdibus")
    , ("kalendis", "kalendīs")
    , ("kalendas", "kalendās")
    , ("kalendas-capitalized", "Kalendas")
    , ("pridie", "prīdiē")
    , ("kal-short", "kal.")
    , ("ante-diem-short", "a.d.")
    , ("non-short", "nōn.")
    , ("id-short", "eīd.")
    , ("pridie-short", "prīd.")
    ]

escapeHtmlAttr :: String -> String
escapeHtmlAttr =
    concatMap
        ( \c -> case c of
            '&' -> "&amp;"
            '"' -> "&quot;"
            '<' -> "&lt;"
            '>' -> "&gt;"
            _ -> [c]
        )
