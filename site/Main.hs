{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Control.Applicative ((<|>))
import Data.Char (isAlphaNum)
import Data.List (intercalate, isPrefixOf, sort, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Ord (Down (Down))
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, formatTime, parseTimeM)
import Hakyll hiding (escapeHtml, isExternal)
import Hakyll.Core.Dependencies (DependencyKind (KindContent))
import Site.Config
import Site.DateScript
import Site.Transliterate
import Site.Types
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (dropExtension, takeExtension, takeFileName, (</>))
import System.IO (hPutStrLn, stderr)
import Text.Pandoc.Definition
import Text.Pandoc.Extensions
import Text.Pandoc.Options
import Text.Pandoc.Walk (walkM)

type SiteCompiler = ReaderT OutputLanguage Compiler

data Site = Site
  { sitePages :: [StaticPage],
    sitePosts :: [BlogPost],
    siteMappings :: M.Map T.Text LanguageMappings,
    siteLocales :: Locales,
    siteOutputTags :: [T.Text],
    siteLogicalPaths :: S.Set FilePath,
    siteAssets :: S.Set FilePath
  }

main :: IO ()
main = do
  pages <- discoverStaticPages "content/pages"
  posts <- discoverBlogPosts "content/blog"
  mappings <- traverse loadMappings transliterationFiles
  locales <- loadLocales "locales.yaml"
  assets <- S.fromList <$> listFilesRecursively "assets"
  let outputTags = discoverOutputTags mappings pages posts
      archives = archivePaths posts
      logicalPaths = S.fromList $ "blog" : archives <> fmap staticLogicalPath pages <> fmap postLogicalPath posts
      site = Site pages posts mappings locales outputTags logicalPaths assets
  validateSite site
  hakyll $ siteRules site

siteRules :: Site -> Rules ()
siteRules site = do
  copyStaticFiles
  match ("content/**.md" .||. "transliterate/*.yaml" .||. "locales.yaml") $ compile getResourceBody
  match "templates/*" $ compile templateBodyCompiler
  contentDependency <- makePatternDependency KindContent "content/**.md"
  transliterationDependency <- makePatternDependency KindContent "transliterate/*.yaml"
  localeDependency <- makePatternDependency KindContent "locales.yaml"
  rulesExtraDependencies [contentDependency, transliterationDependency, localeDependency] $ generatedRules site

generatedRules :: Site -> Rules ()
generatedRules site = do
  forM_ (sitePages site) $ \page ->
    forLanguages site $ \language ->
      create [fromFilePath $ routeFor (staticLogicalPath page) language] $ do
        route idRoute
        compile $ runReaderT (compileStaticPage site page) language

  forM_ (sitePosts site) $ \post ->
    forLanguages site $ \language ->
      create [fromFilePath $ routeFor (postLogicalPath post) language] $ do
        route idRoute
        compile $ runReaderT (compileBlogPost site post) language

  forLanguages site $ \language ->
    create [fromFilePath $ routeFor "blog" language] $ do
      route idRoute
      compile $ compileBlogIndex site language

  forM_ (archivePaths $ sitePosts site) $ \archive ->
    forLanguages site $ \language ->
      create [fromFilePath $ routeFor archive language] $ do
        route idRoute
        compile $ compileArchive site archive language

forLanguages :: Site -> (OutputLanguage -> Rules ()) -> Rules ()
forLanguages site action = forM_ (Default : fmap LanguageTag (siteOutputTags site)) action

compileStaticPage :: Site -> StaticPage -> SiteCompiler (Item String)
compileStaticPage site page = do
  language <- ask
  case sourceFor site (staticSources page) language of
    Nothing -> lift $ compileSelector site (staticLogicalPath page) language (availableTags site $ staticSources page)
    Just source -> lift $ compileAuthored site False (staticLogicalPath page) (staticSources page) Nothing language source

compileBlogPost :: Site -> BlogPost -> SiteCompiler (Item String)
compileBlogPost site post = do
  language <- ask
  case sourceFor site (postSources post) language of
    Nothing -> lift $ compileSelector site (postLogicalPath post) language (availableTags site $ postSources post)
    Just source -> lift $ compileAuthored site True (postLogicalPath post) (postSources post) (Just $ postDate post) language source

data SelectedSource = SelectedSource
  { selectedIdentifier :: Identifier,
    selectedBaseLanguage :: T.Text,
    selectedTransliteration :: Transliteration
  }

sourceFor :: Site -> Sources -> OutputLanguage -> Maybe SelectedSource
sourceFor site sources language = do
  let target = requestedTag language
      base = primaryLanguage target
  identifier <- M.lookup base $ sourceFiles sources
  transliteration <-
    case M.lookup base (siteMappings site) of
      Nothing -> if target == base then Just Identity else Nothing
      Just mappings -> mappingFor mappings target
  pure $ SelectedSource identifier base transliteration

availableTags :: Site -> Sources -> [T.Text]
availableTags site sources =
  [ tag
  | tag <- siteOutputTags site,
    isJust $ sourceFor site sources (LanguageTag tag)
  ]

compileAuthored :: Site -> Bool -> FilePath -> Sources -> Maybe String -> OutputLanguage -> SelectedSource -> Compiler (Item String)
compileAuthored site isPost logicalPath sources date language source = do
  body <- loadBody $ selectedIdentifier source
  metadata <- getMetadata $ selectedIdentifier source
  let input = Item (selectedIdentifier source) body
  parsed <- readPandocWith readerOptions input
  let transform = transcribeString (selectedBaseLanguage source) logicalPath (selectedTransliteration source)
      transliterated = fmap (transliteratePandoc (selectedBaseLanguage source) logicalPath (selectedTransliteration source)) parsed
  linked <- rewriteLinks site (selectedIdentifier source) language transliterated
  let switcher = pageSwitcherHtml site logicalPath language sources
      structured = if isPost then linked else fmap (insertSwitcher switcher) linked
  let rendered = writePandocWith writerOptions structured
      output = Item (fromFilePath $ routeFor logicalPath language) (itemBody rendered)
      context = pageContext logicalPath language metadata transform switcher date
  if isPost
    then loadAndApplyTemplate "templates/post.html" context output >>= loadAndApplyTemplate "templates/default.html" context
    else loadAndApplyTemplate "templates/default.html" context output

insertSwitcher :: String -> Pandoc -> Pandoc
insertSwitcher "" pandoc = pandoc
insertSwitcher switcher (Pandoc meta blocks) = Pandoc meta $ go blocks
  where
    nav = RawBlock (Format "html") $ T.pack $ "<nav class=\"switcher\">\n" <> switcher <> "\n</nav>"
    go [] = [nav]
    go (header@Header { } : rest) = header : nav : rest
    go (block : rest) = block : go rest

pageContext :: FilePath -> OutputLanguage -> Metadata -> (String -> String) -> String -> Maybe String -> Context String
pageContext logicalPath language metadata transform switcher date =
  constField "title" (transform $ fromMaybe (fallbackTitle logicalPath) $ lookupString "title" metadata)
    <> transformedField "description"
    <> transformedField "image-alt"
    <> transformedField "title-meta"
    <> constField "image" (absoluteAsset $ fromMaybe defaultImage $ lookupString "image" metadata)
    <> constField "lang" (T.unpack $ requestedTag language)
    <> constField "author" authorName
    <> constField "copyyear" currentYear
    <> constField "url" (baseUrl <> urlFor logicalPath language)
    <> optionalField "switcher" switcher
    <> maybe mempty (constField "date") date
    <> constField "scripts" (scriptsFor logicalPath language transform)
    <> defaultContext
  where
    transformedField name = maybe mempty (constField name . transform) $ lookupString name metadata

compileSelector :: Site -> FilePath -> OutputLanguage -> [T.Text] -> Compiler (Item String)
compileSelector site logicalPath language tags = do
  let heading = localizedUiText site language "available-languages"
      title = heading <> " | " <> fallbackTitle logicalPath
      choices = availableLanguageLinks site logicalPath tags
      body = "<h1>" <> escapeHtml heading <> "</h1>\n" <> choices
      item = Item (fromFilePath $ routeFor logicalPath language) body
      context = syntheticContext logicalPath language title ""
  loadAndApplyTemplate "templates/default.html" context item

compileBlogIndex :: Site -> OutputLanguage -> Compiler (Item String)
compileBlogIndex site language =
  compileListing site "blog" language (localizedUiText site language "blog") $ sitePosts site

compileArchive :: Site -> FilePath -> OutputLanguage -> Compiler (Item String)
compileArchive site archive language = do
  let descendants =
        filter
          (\post -> archive `isPathPrefixOf` postLogicalPath post && canRender site language (postSources post))
          (sitePosts site)
      allDescendants = filter (\post -> archive `isPathPrefixOf` postLogicalPath post) $ sitePosts site
  case descendants of
    [] -> compileSelector site archive language $ tagsWithPosts site allDescendants
    [post] -> compileRedirect site archive language $ urlFor (postLogicalPath post) language
    posts -> compileListing site archive language (archiveTitle site language archive) posts

compileListing :: Site -> FilePath -> OutputLanguage -> String -> [BlogPost] -> Compiler (Item String)
compileListing site logicalPath language title posts = do
  entries <- fmap concat $ forM (sortOn (Down . postDate) posts) $ \post -> do
    case listingSourceFor site (postSources post) language of
      Nothing -> pure ""
      Just source -> do
        metadata <- getMetadata $ selectedIdentifier source
        let transform = transcribeString (selectedBaseLanguage source) (postLogicalPath post) (selectedTransliteration source)
            postTitle = transform $ fromMaybe (postSlug post) $ lookupString "title" metadata
        pure $
          "<li><a href=\""
            <> urlFor (postLogicalPath post) language
            <> "\">"
            <> escapeHtml postTitle
            <> "</a> <span class=\"post-date\">"
            <> postDate post
            <> "</span></li>\n"
  let body =
        "<h1>" <> escapeHtml title <> "</h1>\n"
          <> if null posts
            then "<p>" <> escapeHtml (localizedUiText site language "no-posts") <> "</p>"
            else "<ul class=\"post-list\">\n" <> entries <> "</ul>"
      item = Item (fromFilePath $ routeFor logicalPath language) body
  loadAndApplyTemplate "templates/default.html" (syntheticContext logicalPath language title "") item

listingSourceFor :: Site -> Sources -> OutputLanguage -> Maybe SelectedSource
listingSourceFor site sources language =
  sourceFor site sources language <|> sourceFor site sources (LanguageTag "en")

compileRedirect :: Site -> FilePath -> OutputLanguage -> String -> Compiler (Item String)
compileRedirect site logicalPath language target =
  makeItem
    ( "<!doctype html><html lang=\""
        <> T.unpack (requestedTag language)
        <> "\"><head><meta charset=\"utf-8\"><meta http-equiv=\"refresh\" content=\"0; url="
        <> target
        <> "\"><link rel=\"canonical\" href=\""
        <> target
        <> "\"></head><body><p><a href=\""
        <> target
        <> "\">"
        <> escapeHtml (localizedUiText site language "continue")
        <> "</a></p></body></html>"
    )
    >>= \item -> pure item {itemIdentifier = fromFilePath $ routeFor logicalPath language}

syntheticContext :: FilePath -> OutputLanguage -> String -> String -> Context String
syntheticContext logicalPath language title scripts =
  constField "title" title
    <> constField "lang" (T.unpack $ requestedTag language)
    <> constField "author" authorName
    <> constField "copyyear" currentYear
    <> constField "url" (baseUrl <> urlFor logicalPath language)
    <> constField "scripts" scripts
    <> defaultContext

optionalField :: String -> String -> Context a
optionalField _ "" = mempty
optionalField name value = constField name value

canRender :: Site -> OutputLanguage -> Sources -> Bool
canRender site language sources = isJust $ sourceFor site sources language

tagsWithPosts :: Site -> [BlogPost] -> [T.Text]
tagsWithPosts site posts =
  [ tag
  | tag <- siteOutputTags site,
    any (canRender site (LanguageTag tag) . postSources) posts
  ]

pageSwitcherHtml :: Site -> FilePath -> OutputLanguage -> Sources -> String
pageSwitcherHtml site logicalPath current sources =
  intercalate "\n" $ catMaybes [languageRow, scriptRow]
  where
    tags = availableTags site sources
    bases = sort . S.toList . S.fromList $ fmap primaryLanguage tags
    currentBase = primaryLanguage $ requestedTag current
    scripts = filter ((== currentBase) . primaryLanguage) tags
    languageRow
      | length bases <= 1 = Nothing
      | otherwise =
          Just $ selectorRow (localizedUiText site current "languages") $ fmap languageLink bases
    scriptRow
      | length scripts <= 1 = Nothing
      | otherwise =
          Just $ selectorRow (localizedUiText site current "scripts") $ fmap scriptLink scripts
    languageLink base =
      let tag = primaryTag tags base
          label = escapeHtml $ languageLabel site tag
       in if currentBase == base
            then "<strong>" <> label <> "</strong>"
            else taggedLink logicalPath tag label
    scriptLink tag =
      let label = escapeHtml $ scriptLabel site tag
       in if requestedTag current == tag
            then "<strong>" <> label <> "</strong>"
            else taggedLink logicalPath tag label

selectorRow :: String -> [String] -> String
selectorRow label links =
  "<p><span>" <> escapeHtml label <> ":</span> " <> intercalate " · " links <> "</p>"

taggedLink :: FilePath -> T.Text -> String -> String
taggedLink logicalPath tag label =
  "<a hreflang=\"" <> T.unpack tag <> "\" href=\"" <> urlFor logicalPath (LanguageTag tag) <> "\">" <> label <> "</a>"

availableLanguageLinks :: Site -> FilePath -> [T.Text] -> String
availableLanguageLinks site logicalPath tags =
  "<ul>\n"
    <> concat
      [ "<li>" <> taggedLink logicalPath tag (escapeHtml $ languageLabel site tag) <> "</li>\n"
      | base <- bases,
        let tag = primaryTag tags base
      ]
    <> "</ul>"
  where
    bases = sort . S.toList . S.fromList $ fmap primaryLanguage tags

primaryTag :: [T.Text] -> T.Text -> T.Text
primaryTag tags base
  | base `elem` tags = base
  | otherwise = fromMaybe base $ listToMaybe $ filter ((== base) . primaryLanguage) tags

languageLabel :: Site -> T.Text -> String
languageLabel site tag =
  let base = primaryLanguage tag
      endonym = maybe tag localeName $ M.lookup base $ siteLocales site
   in renderTextForTag site tag endonym

scriptLabel :: Site -> T.Text -> String
scriptLabel site tag =
  let base = primaryLanguage tag
      name = M.lookup base (siteLocales site) >>= M.lookup tag . localeScripts
   in renderTextForTag site tag $ fromMaybe tag name

localizedUiText :: Site -> OutputLanguage -> T.Text -> String
localizedUiText site language key =
  let tag = requestedTag language
      base = primaryLanguage tag
      value = M.lookup base (siteLocales site) >>= M.lookup key . localeStrings
   in renderTextForTag site tag $ fromMaybe key value

renderTextForTag :: Site -> T.Text -> T.Text -> String
renderTextForTag site tag text =
  let base = primaryLanguage tag
      transliteration = M.lookup base (siteMappings site) >>= (`mappingFor` tag)
   in T.unpack $ case transliteration of
        Nothing -> text
        Just Identity -> text
        Just mapping -> T.pack $ transcribeString base "__ui__" mapping $ T.unpack text

rewriteLinks :: Site -> Identifier -> OutputLanguage -> Item Pandoc -> Compiler (Item Pandoc)
rewriteLinks site source language = traverse $ walkM rewriteInline
  where
    rewriteInline inline =
      case inline of
        Link attrs label (target, title) -> Link attrs label . (,title) <$> rewriteTarget target
        Image attrs label (target, title) -> Image attrs label . (,title) <$> rewriteTarget target
        _ -> pure inline
    rewriteTarget target
      | isExternal target || T.isPrefixOf "#" target = pure target
      | otherwise = do
          let raw = T.unpack target
              (pathPart, suffix) = break (`elem` ("?#" :: String)) raw
              rooted = if "/" `isPrefixOf` pathPart then drop 1 pathPart else pathPart
              logical = trimSlashes rooted
              asset = if "assets/" `isPrefixOf` logical then Just logical else Nothing
          if logical `S.member` siteLogicalPaths site
            then pure $ T.pack $ urlFor logical language <> suffix
            else case asset of
              Just path | path `S.member` siteAssets site -> pure $ T.pack $ "/" <> path <> suffix
              _ -> do
                unsafeCompiler $ hPutStrLn stderr $ "WARNING: " <> show source <> ": unresolved internal link " <> raw
                pure target

isExternal :: T.Text -> Bool
isExternal target =
  any (`T.isPrefixOf` target) ["http://", "https://", "mailto:", "tel:", "data:"]

trimSlashes :: String -> String
trimSlashes = reverse . dropWhile (== '/') . reverse . dropWhile (== '/')

isPathPrefixOf :: FilePath -> FilePath -> Bool
isPathPrefixOf prefix path = prefix == path || (prefix <> "/") `isPrefixOf` path

archiveTitle :: Site -> OutputLanguage -> FilePath -> String
archiveTitle site language path =
  localizedUiText site language "posts-from" <> " " <> fmap (\c -> if c == '/' then '-' else c) path

archivePaths :: [BlogPost] -> [FilePath]
archivePaths = sort . S.toList . S.fromList . concatMap prefixes
  where
    prefixes post =
      case splitOnSlash $ postLogicalPath post of
        year : month : day : _ -> [year, year </> month, year </> month </> day]
        _ -> []

splitOnSlash :: String -> [String]
splitOnSlash "" = []
splitOnSlash value =
  let (part, rest) = break (== '/') value
   in part : case rest of
        [] -> []
        _ : remaining -> splitOnSlash remaining

discoverStaticPages :: FilePath -> IO [StaticPage]
discoverStaticPages root = do
  exists <- doesDirectoryExist root
  unless exists $ fail $ "missing static-page directory: " <> root
  entries <- sort <$> listDirectory root
  catMaybes <$> traverse discover entries
  where
    discover entry = do
      let path = root </> entry
      directory <- doesDirectoryExist path
      file <- doesFileExist path
      if directory
        then Just . StaticPage (logicalName entry) <$> discoverSources path
        else
          if file && takeExtension entry == ".md"
            then do
              language <- readSourceLanguage path
              pure $ Just $ StaticPage (logicalName $ dropExtension entry) $ Sources $ M.singleton language (fromFilePath path)
            else pure Nothing
    logicalName "index" = ""
    logicalName name = name

discoverBlogPosts :: FilePath -> IO [BlogPost]
discoverBlogPosts root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else do
      entries <- sort <$> listDirectory root
      catMaybes <$> traverse discover entries
  where
    discover entry = do
      let path = root </> entry
          name = if takeExtension entry == ".md" then dropExtension entry else entry
      directory <- doesDirectoryExist path
      file <- doesFileExist path
      case parsePostName name of
        Nothing
          | directory || (file && takeExtension entry == ".md") -> fail $ "invalid blog source name: " <> path
          | otherwise -> pure Nothing
        Just (date, slug) -> do
          sources <-
            if directory
              then discoverSources path
              else do
                language <- readSourceLanguage path
                pure $ Sources $ M.singleton language (fromFilePath path)
          let (year, month, day) = dateParts date
              logicalPath = year </> month </> day </> slug
          pure $ Just $ BlogPost logicalPath date slug sources

discoverSources :: FilePath -> IO Sources
discoverSources directory = do
  entries <- sort <$> listDirectory directory
  let markdown = filter ((== ".md") . takeExtension) entries
  when (null markdown) $ fail $ "no language sources in " <> directory
  pairs <- forM markdown $ \entry -> do
    let path = directory </> entry
        language = T.pack $ dropExtension entry
    metadataLanguage <- readSourceLanguage path
    when (metadataLanguage /= language) $
      fail $ path <> ": lang metadata must match filename " <> T.unpack language
    pure (language, fromFilePath path)
  pure $ Sources $ M.fromList pairs

readSourceLanguage :: FilePath -> IO T.Text
readSourceLanguage path = do
  contents <- lines <$> readFile path
  let frontMatter = takeWhile (/= "---") $ drop 1 $ dropWhile (/= "---") contents
      languages = [T.strip $ T.pack $ drop 5 line | line <- frontMatter, "lang:" `isPrefixOf` line]
  case languages of
    [language] | not (T.null language) -> pure language
    _ -> fail $ path <> ": expected exactly one lang field in front matter"

parsePostName :: String -> Maybe (String, String)
parsePostName name = do
  let (dateText, slugWithDash) = splitAt 10 name
  guardValue (length name > 11 && take 1 slugWithDash == "-")
  day <- parseTimeM True defaultTimeLocale "%F" dateText :: Maybe Day
  let slug = drop 1 slugWithDash
  guardValue $ not (null slug) && all (\c -> isAlphaNum c || c == '-') slug
  pure (formatTime defaultTimeLocale "%F" day, slug)

guardValue :: Bool -> Maybe ()
guardValue True = Just ()
guardValue False = Nothing

dateParts :: String -> (String, String, String)
dateParts date = (take 4 date, take 2 $ drop 5 date, take 2 $ drop 8 date)

discoverOutputTags :: M.Map T.Text LanguageMappings -> [StaticPage] -> [BlogPost] -> [T.Text]
discoverOutputTags mappings pages posts =
  sort . S.toList . S.fromList $ concatMap tagsForBase allBases
  where
    allSources = fmap staticSources pages <> fmap postSources posts
    allBases = concatMap (M.keys . sourceFiles) allSources
    tagsForBase base = maybe [base] mappingTags $ M.lookup base mappings

validateSite :: Site -> IO ()
validateSite site = do
  let logicalPaths = "blog" : archivePaths (sitePosts site) <> fmap staticLogicalPath (sitePages site) <> fmap postLogicalPath (sitePosts site)
      duplicatePaths = duplicateValues logicalPaths
      requiredStrings = ["languages", "scripts", "available-languages", "blog", "no-posts", "posts-from", "continue"]
      missingLocales =
        [ base
        | base <- S.toList $ S.fromList $ fmap primaryLanguage $ siteOutputTags site,
          M.notMember base $ siteLocales site
        ]
      missingStrings =
        [ (base, key)
        | (base, locale) <- M.toList $ siteLocales site,
          key <- requiredStrings,
          M.notMember key $ localeStrings locale
        ]
      missingScripts =
        [ tag
        | tag <- siteOutputTags site,
          let base = primaryLanguage tag,
          maybe True (M.notMember tag . localeScripts) $ M.lookup base $ siteLocales site
        ]
      routes =
        [ routeFor logical language
        | logical <- S.toList $ siteLogicalPaths site,
          language <- Default : fmap LanguageTag (siteOutputTags site)
        ]
      duplicates = duplicateValues routes
  unless (null duplicatePaths) $ fail $ "duplicate logical paths: " <> show duplicatePaths
  unless (null duplicates) $ fail $ "duplicate output routes: " <> show duplicates
  unless (null missingLocales) $ fail $ "missing locales: " <> show missingLocales
  unless (null missingStrings) $ fail $ "missing localized strings: " <> show missingStrings
  unless (null missingScripts) $ fail $ "missing localized script names: " <> show missingScripts

duplicateValues :: Ord a => [a] -> [a]
duplicateValues values = M.keys $ M.filter (> (1 :: Int)) $ M.fromListWith (+) [(value, 1) | value <- values]

listFilesRecursively :: FilePath -> IO [FilePath]
listFilesRecursively root = do
  entries <- listDirectory root
  fmap concat $ forM entries $ \entry -> do
    let path = root </> entry
    directory <- doesDirectoryExist path
    if directory then listFilesRecursively path else pure [path]

copyStaticFiles :: Rules ()
copyStaticFiles = do
  match "assets/**" $ route idRoute >> compile copyFileCompiler
  match ".well-known/**" $ route idRoute >> compile copyFileCompiler
  match (fromList ["CNAME", ".nojekyll"]) $ route idRoute >> compile copyFileCompiler

readerOptions :: ReaderOptions
readerOptions =
  defaultHakyllReaderOptions
    { readerExtensions =
        disableExtension Ext_implicit_figures $
          enableExtension Ext_definition_lists $
            enableExtension Ext_bracketed_spans $
              enableExtension Ext_fenced_divs githubMarkdownExtensions
    }

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions

absoluteAsset :: String -> String
absoluteAsset value
  | "/" `isPrefixOf` value || isExternal (T.pack value) = value
  | otherwise = "/" <> value

fallbackTitle :: FilePath -> String
fallbackTitle "" = "Home"
fallbackTitle path = takeFileName path

escapeHtml :: String -> String
escapeHtml = concatMap escape
  where
    escape '&' = "&amp;"
    escape '<' = "&lt;"
    escape '>' = "&gt;"
    escape '"' = "&quot;"
    escape c = [c]
