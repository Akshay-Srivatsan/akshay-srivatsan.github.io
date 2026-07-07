{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Control.Monad.State
import Data.List (intercalate)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Hakyll
import Site.Config
import Site.DateScript
import Site.Transliterate
import Site.Types
import System.FilePath (replaceExtension)
import Text.Pandoc.Options

type PageMap = M.Map String [PageSpec]

main :: IO ()
main = do
  blogPages <- discoverBlogPages
  hakyll $ do
    copyStaticFiles
    compileSourcesAndTemplates

    let pages = fixedPages <> blogPages
        pagesByGroup = M.fromListWith (flip (<>)) [(pageGroup p, [p]) | p <- pages]
    overrides <- preprocess loadTransliterationOverrides

    forM_ pages $ \page -> do
      mappings <- preprocess $ loadMappingsForPage page
      forM_ (pageVariants page) $ \variant ->
        create [fromFilePath $ outputFor page variant] $ do
          route idRoute
          compile $ compilePage pagesByGroup page mappings overrides variant

    create ["blog.html"] $ do
      route idRoute
      compile $
        makeItem ("" :: String)
          >>= loadAndApplyTemplate "templates/blog.html" blogContext
          >>= loadAndApplyTemplate "templates/default.html" blogContext

copyStaticFiles :: Rules ()
copyStaticFiles = do
  match ("assets/**" .&&. complement "assets/links.md" .&&. complement "assets/js/**") $ do
    route idRoute
    compile copyFileCompiler

  match "assets/js/date.js" $ do
    route idRoute
    compile copyFileCompiler

  match ".well-known/**" $ do
    route idRoute
    compile copyFileCompiler

  match (fromList ["CNAME", ".nojekyll"]) $ do
    route idRoute
    compile copyFileCompiler

compileSourcesAndTemplates :: Rules ()
compileSourcesAndTemplates = do
  match "content/**.md" $
    compile getResourceBody

  match "templates/*" $
    compile templateBodyCompiler

blogContext :: Context String
blogContext =
  constField "posts" ""
    <> constField "title" "Blog | Akshay Srivatsan"
    <> constField "lang" "en"
    <> constField "author" authorName
    <> constField "copyyear" currentYear
    <> constField "url" (baseUrl <> "/blog.html")
    <> constField "alternateLinks" ""
    <> constField "scripts" ""
    <> defaultContext

compilePage :: PageMap -> PageSpec -> MappingSet -> TransliterationOverrides -> Variant -> Compiler (Item String)
compilePage pagesByGroup page mappings overrides variant = do
  body <- loadBody (fromFilePath $ pageSource page)
  links <- unsafeCompiler $ readFile "assets/links.md"
  rendered <- renderMarkdown page body links
  let renderedForTarget = Item (fromFilePath $ outputFor page variant) (itemBody rendered)
  transformed <- applyTemplates pagesByGroup page variant mappings overrides renderedForTarget
  saveSnapshotWhenBlog page transformed

renderMarkdown :: PageSpec -> String -> String -> Compiler (Item String)
renderMarkdown page body links = do
  let sourceItem = Item (fromFilePath $ replaceExtension (pageOutput page) "md") (stripLegacyControls body <> "\n\n" <> links)
  rendered <- writePandocWith writerOptions <$> readPandocWith readerOptions sourceItem
  pure $ Item (fromFilePath $ pageOutput page) (itemBody rendered)

applyTemplates :: PageMap -> PageSpec -> Variant -> MappingSet -> TransliterationOverrides -> Item String -> Compiler (Item String)
applyTemplates pagesByGroup page variant mappings overrides item = do
  metadata <- getMetadata (fromFilePath $ pageSource page)
  let ctx = pageContext pagesByGroup page variant mappings overrides metadata
      switcher = switcherHtmlMaybe pagesByGroup page mappings overrides variant
      item' =
        fmap
          ( (if isBlogPage page then id else injectSwitcherAfterTitle switcher)
              . transformHtml page mappings overrides variant
          )
          item
  if isBlogPage page
    then
      loadAndApplyTemplate "templates/post.html" ctx item'
        >>= loadAndApplyTemplate "templates/default.html" ctx
    else
      loadAndApplyTemplate "templates/default.html" ctx item'

pageContext :: PageMap -> PageSpec -> Variant -> MappingSet -> TransliterationOverrides -> Metadata -> Context String
pageContext pagesByGroup page variant mappings overrides metadata =
  constField "title" title
    <> maybe mempty (constField "description") description
    <> maybe mempty (constField "image-alt") imageAlt
    <> maybe mempty (constField "title-meta") titleMeta
    <> constField "image" image
    <> constField "lang" (variantLang variant)
    <> constField "author" authorName
    <> constField "copyyear" currentYear
    <> constField "url" (baseUrl <> "/" <> outputFor page variant)
    <> constField "alternateLinks" ""
    <> maybe mempty (constField "switcher") switcher
    <> constField "scripts" (scriptsFor page mappings variant)
    <> defaultContext
  where
    transform = transformFor page mappings variant
    title = transform $ fromMaybe "" $ lookupString "title" metadata
    description = transform <$> lookupString "description" metadata
    image = fromMaybe "assets/img/portrait-small.jpg" $ lookupString "image" metadata
    imageAlt = transform <$> lookupString "image-alt" metadata
    titleMeta = transform <$> lookupString "title-meta" metadata
    switcher =
      switcherHtmlMaybe pagesByGroup page mappings overrides variant

switcherHtmlMaybe :: PageMap -> PageSpec -> MappingSet -> TransliterationOverrides -> Variant -> Maybe String
switcherHtmlMaybe pagesByGroup page mappings overrides variant =
  let html = switcherHtml pagesByGroup page mappings overrides variant
   in if null html then Nothing else Just html

injectSwitcherAfterTitle :: Maybe String -> String -> String
injectSwitcherAfterTitle Nothing body = body
injectSwitcherAfterTitle (Just switcher) body =
  case breakOn titleEnd body of
    Nothing -> body
    Just (before, after) -> before <> titleEnd <> "\n<nav class=\"switcher\">\n" <> switcher <> "\n</nav>" <> drop (length titleEnd) after
  where
    titleEnd = "</h1>"

saveSnapshotWhenBlog :: PageSpec -> Item String -> Compiler (Item String)
saveSnapshotWhenBlog page item =
  if isBlogPage page && pageOutput page == outputFor page (defaultVariant page)
    then saveSnapshot "content" item
    else pure item

readerOptions :: ReaderOptions
readerOptions =
  defaultHakyllReaderOptions
    { readerExtensions = snd $ runState customExtensions githubMarkdownExtensions
    }

customExtensions :: State Extensions ()
customExtensions = do
  modify $ enableExtension Ext_fenced_divs
  modify $ enableExtension Ext_bracketed_spans
  modify $ enableExtension Ext_definition_lists
  modify $ disableExtension Ext_implicit_figures

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions

switcherHtml :: PageMap -> PageSpec -> MappingSet -> TransliterationOverrides -> Variant -> String
switcherHtml pagesByGroup page mappings overrides variant =
  transformHtml page mappings overrides variant $ languageSwitcher <> scriptSwitcher
  where
    languageSwitcher
      | showLanguageSwitcher pagesByGroup page =
          switcherRow page "languages" (languageLinks pagesByGroup page)
      | otherwise = ""
    scriptSwitcher
      | length (pageVariants page) > 1 =
          switcherRow page "scripts" (variantLinks page variant)
      | otherwise = ""

showLanguageSwitcher :: PageMap -> PageSpec -> Bool
showLanguageSwitcher pagesByGroup page =
  length pages > 1
  where
    pages = M.findWithDefault [page] (pageGroup page) pagesByGroup

switcherRow :: PageSpec -> String -> [String] -> String
switcherRow page key links =
  "<p>"
    <> label
    <> ": "
    <> intercalate " · " links
    <> "</p>"
  where
    label = switcherLabel page key

switcherLabel :: PageSpec -> String -> String
switcherLabel page key =
  case (pageLanguageLabel page, key) of
    ("Tamil", "languages") -> "மொழிகள்"
    ("Tamil", "scripts") -> "எழுத்து முறைகள்"
    ("Hindi", "languages") -> "भाषाएँ"
    ("Hindi", "scripts") -> "लिपियाँ"
    ("Sanskrit", "languages") -> "bhāṣāḥ"
    ("Sanskrit", "scripts") -> "lipayaḥ"
    ("Latin", "languages") -> "linguae"
    ("Latin", "scripts") -> "litterae"
    (_, "languages") -> "Languages"
    _ -> "Scripts"

languageLinks :: PageMap -> PageSpec -> [String]
languageLinks pagesByGroup page =
  [ linkFor other (localizedLanguageName page other)
  | other <- M.findWithDefault [page] (pageGroup page) pagesByGroup
  ]
  where
    linkFor other label =
      if pageOutput other == pageOutput page
        then "<strong>" <> label <> "</strong>"
        else "<a href=\"/" <> pageOutput other <> "\">" <> label <> "</a>"

localizedLanguageName :: PageSpec -> PageSpec -> String
localizedLanguageName current target =
  case (pageLanguageLabel current, pageLanguageLabel target) of
    ("Tamil", "English") -> "ஆங்கிலம்"
    ("Tamil", "Latin") -> "லத்தீன்"
    ("Tamil", "Tamil") -> "தமிழ்"
    ("Tamil", "Sanskrit") -> "சம்ஸ்கிருதம்"
    ("Tamil", "Hindi") -> "இந்தி"
    ("Hindi", "English") -> "आंग्रेज़ी"
    ("Hindi", "Latin") -> "लातिन"
    ("Hindi", "Tamil") -> "तमिल"
    ("Hindi", "Sanskrit") -> "संस्कृत"
    ("Hindi", "Hindi") -> "हिन्दी"
    ("Sanskrit", "English") -> "āṅglabhāṣā"
    ("Sanskrit", "Latin") -> "lātinbhāṣā"
    ("Sanskrit", "Tamil") -> "tamiḷbhāṣā"
    ("Sanskrit", "Sanskrit") -> "saṃskṛtam"
    ("Sanskrit", "Hindi") -> "hindī"
    ("Latin", "English") -> "Anglica"
    ("Latin", "Latin") -> "Latina"
    ("Latin", "Tamil") -> "Tamulica"
    ("Latin", "Sanskrit") -> "Sanscrita"
    ("Latin", "Hindi") -> "Hindica"
    (_, label) -> label

variantLinks :: PageSpec -> Variant -> [String]
variantLinks page current =
  [ if v == current
      then "<strong>" <> variantLabel v <> "</strong>"
      else "<a href=\"/" <> outputFor page v <> "\">" <> variantLabel v <> "</a>"
  | v <- pageVariants page
  ]

stripLegacyControls :: String -> String
stripLegacyControls =
  stripBlock "<div id=\"scripts\"" "</div>"
    . stripBlock "<div id=\"font\"" "</div>"
    . stripBlock "<script>" "</script>"

stripBlock :: String -> String -> String -> String
stripBlock start end input =
  case breakOn start input of
    Nothing -> input
    Just (before, rest) ->
      case breakOn end rest of
        Nothing -> before
        Just (_, afterEnd) -> before <> stripBlock start end (drop (length end) afterEnd)

breakOn :: String -> String -> Maybe (String, String)
breakOn needle haystack = go "" haystack
  where
    go _ "" = Nothing
    go before rest@(x : xs)
      | needle `prefixOf` rest = Just (reverse before, rest)
      | otherwise = go (x : before) xs

prefixOf :: String -> String -> Bool
prefixOf prefix s = take (length prefix) s == prefix
