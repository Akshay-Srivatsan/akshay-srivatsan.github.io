{-# LANGUAGE OverloadedStrings #-}

module Site.Content
  ( BlogPost (..),
    Sources (..),
    StaticPage (..),
    archivePaths,
    discoverBlogPosts,
    discoverStaticPages,
  )
where

import Control.Monad (foldM, unless, when)
import Data.Char (isAlphaNum)
import Data.List (sort)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, formatTime, parseTimeM)
import Hakyll (Identifier, Metadata, lookupString, toFilePath)
import System.FilePath (dropExtension, splitDirectories, takeExtension, (</>))

newtype Sources = Sources
  { sourceFiles :: M.Map T.Text Identifier
  }
  deriving (Eq, Show)

data StaticPage = StaticPage
  { staticLogicalPath :: FilePath,
    staticSources :: Sources
  }
  deriving (Eq, Show)

data BlogPost = BlogPost
  { postLogicalPath :: FilePath,
    postDate :: String,
    postSlug :: String,
    postSources :: Sources
  }
  deriving (Eq, Show)

discoverStaticPages :: [(Identifier, Metadata)] -> Either String [StaticPage]
discoverStaticPages entries =
  fmap (uncurry StaticPage . fmap Sources) . M.toAscList
    <$> foldM addPage M.empty entries
  where
    addPage pages (identifier, metadata) = do
      (logicalPath, language) <- staticSource identifier metadata
      sources <- insertSource identifier language $ M.findWithDefault M.empty logicalPath pages
      pure $ M.insert logicalPath sources pages

discoverBlogPosts :: [(Identifier, Metadata)] -> Either String [BlogPost]
discoverBlogPosts entries = do
  grouped <- foldM addPost M.empty entries
  traverse makePost $ M.toAscList grouped
  where
    addPost posts (identifier, metadata) = do
      (name, language) <- blogSource identifier metadata
      sources <- insertSource identifier language $ M.findWithDefault M.empty name posts
      pure $ M.insert name sources posts
    makePost (name, sources) =
      case parsePostName name of
        Nothing -> Left $ "invalid blog source name: content/blog/" <> name
        Just (date, slug) ->
          let (year, month, day) = dateParts date
           in Right $ BlogPost (year </> month </> day </> slug) date slug (Sources sources)

staticSource :: Identifier -> Metadata -> Either String (FilePath, T.Text)
staticSource identifier metadata =
  case relativeParts "content/pages" identifier of
    [file]
      | takeExtension file == ".md" ->
          (,)
            (logicalName $ dropExtension file)
            <$> sourceLanguage identifier metadata
    [directory, file]
      | takeExtension file == ".md" -> do
          language <- sourceLanguage identifier metadata
          let filenameLanguage = T.pack $ dropExtension file
          unless (language == filenameLanguage) $
            Left $
              toFilePath identifier
                <> ": lang metadata must match filename "
                <> T.unpack filenameLanguage
          pure (logicalName directory, language)
    _ -> Left $ "invalid static-page source: " <> toFilePath identifier
  where
    logicalName "index" = ""
    logicalName name = name

blogSource :: Identifier -> Metadata -> Either String (FilePath, T.Text)
blogSource identifier metadata =
  case relativeParts "content/blog" identifier of
    [file]
      | takeExtension file == ".md" ->
          (,)
            (dropExtension file)
            <$> sourceLanguage identifier metadata
    [directory, file]
      | takeExtension file == ".md" -> do
          language <- sourceLanguage identifier metadata
          let filenameLanguage = T.pack $ dropExtension file
          unless (language == filenameLanguage) $
            Left $
              toFilePath identifier
                <> ": lang metadata must match filename "
                <> T.unpack filenameLanguage
          pure (directory, language)
    _ -> Left $ "invalid blog source: " <> toFilePath identifier

relativeParts :: FilePath -> Identifier -> [FilePath]
relativeParts root identifier =
  case splitDirectories $ toFilePath identifier of
    first : second : rest
      | [first, second] == splitDirectories root -> rest
    _ -> []

sourceLanguage :: Identifier -> Metadata -> Either String T.Text
sourceLanguage identifier metadata =
  case T.strip . T.pack <$> lookupString "lang" metadata of
    Just language | not (T.null language) -> Right language
    _ -> Left $ toFilePath identifier <> ": expected exactly one lang field in front matter"

insertSource :: Identifier -> T.Text -> M.Map T.Text Identifier -> Either String (M.Map T.Text Identifier)
insertSource identifier language sources = do
  when (M.member language sources) $
    Left $
      "duplicate "
        <> T.unpack language
        <> " source: "
        <> toFilePath identifier
  pure $ M.insert language identifier sources

parsePostName :: String -> Maybe (String, String)
parsePostName name = do
  let (dateText, slugWithDash) = splitAt 10 name
  guardValue (length name > 11 && take 1 slugWithDash == "-")
  day <- parseTimeM True defaultTimeLocale "%F" dateText :: Maybe Day
  let slug = drop 1 slugWithDash
  guardValue $ not (null slug) && all (\character -> isAlphaNum character || character == '-') slug
  pure (formatTime defaultTimeLocale "%F" day, slug)

guardValue :: Bool -> Maybe ()
guardValue True = Just ()
guardValue False = Nothing

dateParts :: String -> (String, String, String)
dateParts date = (take 4 date, take 2 $ drop 5 date, take 2 $ drop 8 date)

archivePaths :: [BlogPost] -> [FilePath]
archivePaths = sort . S.toList . S.fromList . concatMap prefixes
  where
    prefixes post =
      case splitDirectories $ postLogicalPath post of
        year : month : day : _ -> [year, year </> month, year </> month </> day]
        _ -> []
