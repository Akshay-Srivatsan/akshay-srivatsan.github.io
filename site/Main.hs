{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, unless)
import Data.List (isPrefixOf, sort)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Hakyll hiding (isExternal)
import Hakyll qualified as H
import Hakyll.Core.Dependencies (DependencyKind (KindContent))
import Site.Config
import Site.Content
import Site.DateScript
import Site.Transliterate
import Site.Types
import System.FilePath (takeFileName)
import System.IO (hPutStrLn, stderr)
import Text.Pandoc.Definition
import Text.Pandoc.Extensions
import Text.Pandoc.Options
import Text.Pandoc.Walk (walkM)

data Site = Site
  { sitePages :: [StaticPage],
    sitePosts :: [BlogPost],
    siteMappings :: M.Map T.Text LanguageMappings,
    siteLocales :: Locales,
    siteOutputTags :: [T.Text]
  }

main :: IO ()
main = hakyll siteRules

siteRules :: Rules ()
siteRules = do
  pageMetadata <- getAllMetadata "content/pages/**.md"
  postMetadata <- getAllMetadata "content/blog/**.md"
  pages <- either fail pure $ discoverStaticPages pageMetadata
  posts <- either fail pure $ discoverBlogPosts postMetadata
  mappings <- preprocess $ traverse loadMappings transliterationFiles
  locales <- preprocess $ loadLocales "locales.yaml"
  let outputTags = discoverOutputTags mappings pages posts
      site = Site pages posts mappings locales outputTags
  preprocess $ validateSite site
  copyStaticFiles
  match ("content/**.md" .||. "transliterate/*.yaml" .||. "locales.yaml") $ compile getResourceBody
  match "templates/*" $ compile templateBodyCompiler
  transliterationDependency <- makePatternDependency KindContent "transliterate/*.yaml"
  localeDependency <- makePatternDependency KindContent "locales.yaml"
  rulesExtraDependencies [transliterationDependency, localeDependency] $ generatedRules site

generatedRules :: Site -> Rules ()
generatedRules site = do
  forM_ (sitePages site) $ \page ->
    forLanguages site $ \language ->
      create [fromFilePath $ routeFor (staticLogicalPath page) language] $ do
        route idRoute
        compile $ compileStaticPage site page language

  forM_ (sitePosts site) $ \post ->
    forLanguages site $ \language ->
      create [fromFilePath $ routeFor (postLogicalPath post) language] $ do
        route idRoute
        compile $ compileBlogPost site post language

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

compileStaticPage :: Site -> StaticPage -> OutputLanguage -> Compiler (Item String)
compileStaticPage site page language =
  case sourceFor site (staticSources page) language of
    Nothing -> compileSelector site (staticLogicalPath page) language (availableTags site $ staticSources page)
    Just source -> compileAuthored site False (staticLogicalPath page) (staticSources page) language source

compileBlogPost :: Site -> BlogPost -> OutputLanguage -> Compiler (Item String)
compileBlogPost site post language =
  case sourceFor site (postSources post) language of
    Nothing -> compileSelector site (postLogicalPath post) language (availableTags site $ postSources post)
    Just source -> compileAuthored site True (postLogicalPath post) (postSources post) language source

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

compileAuthored :: Site -> Bool -> FilePath -> Sources -> OutputLanguage -> SelectedSource -> Compiler (Item String)
compileAuthored site isPost logicalPath sources language source = do
  switcher <- renderSwitcher site logicalPath language sources
  let transform = transcribeString (selectedBaseLanguage source) logicalPath (selectedTransliteration source)
      transformItem item = do
        let transliterated =
              fmap
                (transliteratePandoc (selectedBaseLanguage source) logicalPath (selectedTransliteration source))
                item
        linked <- rewriteLinks (selectedIdentifier source) language transliterated
        pure $ if isPost then linked else fmap (insertSwitcher switcher) linked
  rendered <-
    load (selectedIdentifier source)
      >>= renderPandocItemWithTransformM readerOptions writerOptions transformItem
  let context = pageContext logicalPath language transform switcher isPost
      target = fromFilePath $ routeFor logicalPath language
  templated <-
    if isPost
      then loadAndApplyTemplate "templates/post.html" context rendered >>= loadAndApplyTemplate "templates/default.html" context
      else loadAndApplyTemplate "templates/default.html" context rendered
  pure templated {itemIdentifier = target}

insertSwitcher :: String -> Pandoc -> Pandoc
insertSwitcher "" pandoc = pandoc
insertSwitcher switcher (Pandoc meta blocks) = Pandoc meta $ go blocks
  where
    nav = RawBlock (Format "html") $ T.pack $ "<nav class=\"switcher\">\n" <> switcher <> "\n</nav>"
    go [] = [nav]
    go (header@Header {} : rest) = header : nav : rest
    go (block : rest) = block : go rest

pageContext :: FilePath -> OutputLanguage -> (String -> String) -> String -> Bool -> Context String
pageContext logicalPath language transform switcher isPost =
  field "title" transformedTitle
    <> field "image" imageField
    <> mapContextBy (`elem` ["description", "image-alt", "title-meta"]) transform metadataField
    <> constField "lang" (T.unpack $ requestedTag language)
    <> constField "author" authorName
    <> constField "copyyear" currentYear
    <> constField "url" (baseUrl <> urlFor logicalPath language)
    <> optionalField "switcher" switcher
    <> ifContext isPost (dateField "date" "%F")
    <> constField "scripts" (scriptsFor logicalPath language transform)
    <> defaultContext
  where
    transformedTitle item =
      transform . fromMaybe (fallbackTitle logicalPath)
        <$> getMetadataField (itemIdentifier item) "title"
    imageField item =
      absoluteAsset . fromMaybe defaultImage
        <$> getMetadataField (itemIdentifier item) "image"

ifContext :: Bool -> Context a -> Context a
ifContext True context = context
ifContext False _ = mempty

data NavigationLink = NavigationLink
  { navigationLabel :: String,
    navigationLanguage :: T.Text,
    navigationCurrent :: Bool
  }

data ListingEntry = ListingEntry
  { listingTitle :: String,
    listingUrl :: String
  }

compileSelector :: Site -> FilePath -> OutputLanguage -> [T.Text] -> Compiler (Item String)
compileSelector site logicalPath language tags = do
  let heading = localizedUiText site language "available-languages"
      title = heading <> " | " <> fallbackTitle logicalPath
      bases = sort . S.toList . S.fromList $ fmap primaryLanguage tags
      choices =
        [ navigationItem logicalPath tag (languageLabel site tag) False
        | base <- bases,
          let tag = primaryTag tags base
        ]
      context =
        constField "heading" (escapeHtml heading)
          <> listField "choices" navigationContext (pure choices)
          <> syntheticContext logicalPath language title ""
  makeItem ""
    >>= loadAndApplyTemplate "templates/selector.html" context
    >>= loadAndApplyTemplate "templates/default.html" context

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
  unsorted <- fmap catMaybes $ forM posts $ \post -> do
    case listingSourceFor site (postSources post) language of
      Nothing -> pure Nothing
      Just source -> do
        metadata <- getMetadata $ selectedIdentifier source
        let transform = transcribeString (selectedBaseLanguage source) (postLogicalPath post) (selectedTransliteration source)
            postTitle = transform $ fromMaybe (postSlug post) $ lookupString "title" metadata
            entry = ListingEntry (escapeHtml postTitle) $ urlFor (postLogicalPath post) language
        pure $ Just $ Item (selectedIdentifier source) entry
  entries <- recentFirst unsorted
  let context =
        constField "heading" (escapeHtml title)
          <> constField "empty" (escapeHtml $ localizedUiText site language "no-posts")
          <> boolField "has-posts" (const $ not $ null entries)
          <> listField "posts" listingContext (pure entries)
          <> syntheticContext logicalPath language title ""
  makeItem ""
    >>= loadAndApplyTemplate "templates/listing.html" context
    >>= loadAndApplyTemplate "templates/default.html" context

listingSourceFor :: Site -> Sources -> OutputLanguage -> Maybe SelectedSource
listingSourceFor site sources language =
  sourceFor site sources language <|> sourceFor site sources (LanguageTag "en")

compileRedirect :: Site -> FilePath -> OutputLanguage -> String -> Compiler (Item String)
compileRedirect site _ language target =
  (makeItem "" :: Compiler (Item String))
    >>= loadAndApplyTemplate
      "templates/redirect.html"
      ( constField "lang" (T.unpack $ requestedTag language)
          <> constField "target" target
          <> constField "continue" (escapeHtml $ localizedUiText site language "continue")
      )

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

renderSwitcher :: Site -> FilePath -> OutputLanguage -> Sources -> Compiler String
renderSwitcher site logicalPath current sources
  | null languages && null scripts = pure ""
  | otherwise = do
      rendered <-
        (makeItem "" :: Compiler (Item String))
          >>= loadAndApplyTemplate
            "templates/switcher.html"
            ( constField "language-label" (escapeHtml $ localizedUiText site current "languages")
                <> constField "script-label" (escapeHtml $ localizedUiText site current "scripts")
                <> boolField "has-languages" (const $ not $ null languages)
                <> boolField "has-scripts" (const $ not $ null scripts)
                <> listField "languages" navigationContext (pure languages)
                <> listField "scripts" navigationContext (pure scripts)
            )
      pure $ itemBody rendered
  where
    tags = availableTags site sources
    bases = sort . S.toList . S.fromList $ fmap primaryLanguage tags
    currentBase = primaryLanguage $ requestedTag current
    languageTags = [primaryTag tags base | base <- bases]
    languages =
      [ navigationItem logicalPath tag (languageLabel site tag) (currentBase == primaryLanguage tag)
      | tag <- languageTags,
        length bases > 1
      ]
    scriptTags = filter ((== currentBase) . primaryLanguage) tags
    scripts =
      [ navigationItem logicalPath tag (scriptLabel site tag) (requestedTag current == tag)
      | tag <- scriptTags,
        length scriptTags > 1
      ]

navigationItem :: FilePath -> T.Text -> String -> Bool -> Item NavigationLink
navigationItem logicalPath tag label current =
  Item
    (fromFilePath $ routeFor logicalPath $ LanguageTag tag)
    (NavigationLink (escapeHtml label) tag current)

navigationContext :: Context NavigationLink
navigationContext =
  field "label" (pure . navigationLabel . itemBody)
    <> field "lang" (pure . T.unpack . navigationLanguage . itemBody)
    <> boolField "current" (navigationCurrent . itemBody)
    <> field "url" routedUrl
  where
    routedUrl item = do
      target <- getRoute $ itemIdentifier item
      maybe (fail $ "missing route for " <> show (itemIdentifier item)) (pure . ("/" <>)) target

listingContext :: Context ListingEntry
listingContext =
  field "title" (pure . listingTitle . itemBody)
    <> field "url" (pure . listingUrl . itemBody)
    <> dateField "date" "%F"

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

rewriteLinks :: Identifier -> OutputLanguage -> Item Pandoc -> Compiler (Item Pandoc)
rewriteLinks source language = traverse $ walkM rewriteInline
  where
    rewriteInline inline =
      case inline of
        Link attrs label (target, title) -> Link attrs label . (,title) <$> rewriteTarget target
        Image attrs label (target, title) -> Image attrs label . (,title) <$> rewriteTarget target
        _ -> pure inline
    rewriteTarget target
      | isExternalUrl target || T.isPrefixOf "#" target = pure target
      | otherwise = do
          let raw = T.unpack target
              (pathPart, suffix) = break (`elem` ("?#" :: String)) raw
              rooted = if "/" `isPrefixOf` pathPart then drop 1 pathPart else pathPart
              logical = trimSlashes rooted
              asset = if "assets/" `isPrefixOf` logical then Just logical else Nothing
              identifier =
                fromFilePath $
                  case asset of
                    Just path -> path
                    Nothing -> routeFor logical language
          resolvedRoute <- getRoute identifier
          case resolvedRoute of
            Just routePath ->
              pure . T.pack $
                case asset of
                  Just _ -> "/" <> routePath <> suffix
                  Nothing -> urlFor logical language <> suffix
            Nothing -> do
              unsafeCompiler $ hPutStrLn stderr $ "WARNING: " <> show source <> ": unresolved internal link " <> raw
              pure target

isExternalUrl :: T.Text -> Bool
isExternalUrl target =
  H.isExternal (T.unpack target)
    || any (`T.isPrefixOf` target) ["mailto:", "tel:", "data:"]

trimSlashes :: String -> String
trimSlashes = reverse . dropWhile (== '/') . reverse . dropWhile (== '/')

isPathPrefixOf :: FilePath -> FilePath -> Bool
isPathPrefixOf prefix path = prefix == path || (prefix <> "/") `isPrefixOf` path

archiveTitle :: Site -> OutputLanguage -> FilePath -> String
archiveTitle site language path =
  localizedUiText site language "posts-from" <> " " <> fmap (\c -> if c == '/' then '-' else c) path

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
        | logical <- logicalPaths,
          language <- Default : fmap LanguageTag (siteOutputTags site)
        ]
      duplicates = duplicateValues routes
  unless (null duplicatePaths) $ fail $ "duplicate logical paths: " <> show duplicatePaths
  unless (null duplicates) $ fail $ "duplicate output routes: " <> show duplicates
  unless (null missingLocales) $ fail $ "missing locales: " <> show missingLocales
  unless (null missingStrings) $ fail $ "missing localized strings: " <> show missingStrings
  unless (null missingScripts) $ fail $ "missing localized script names: " <> show missingScripts

duplicateValues :: (Ord a) => [a] -> [a]
duplicateValues values = M.keys $ M.filter (> (1 :: Int)) $ M.fromListWith (+) [(value, 1) | value <- values]

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
  | "/" `isPrefixOf` value || H.isExternal value = value
  | otherwise = "/" <> value

fallbackTitle :: FilePath -> String
fallbackTitle "" = "Home"
fallbackTitle path = takeFileName path
