{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Hakyll
import Site.Config
import Site.DateScript
import Site.Transliterate
import Site.Types
import Text.Pandoc.Options

type PageMap = M.Map String [PageSpec]

main :: IO ()
main = do
    blogPages <- discoverBlogPages
    hakyll $ do
        copyStaticFiles
        compileSourcesAndTemplates

        let pages = fixedPages <> blogPages
            pagesByGroup = M.fromListWith (<>) [(pageGroup p, [p]) | p <- pages]

        forM_ pages $ \page -> do
            mappings <- preprocess $ loadMappingsForPage page
            forM_ (pageVariants page) $ \variant ->
                create [fromFilePath $ outputFor page variant] $ do
                    route idRoute
                    compile $ compilePage pagesByGroup page mappings variant

        create ["blog.html"] $ do
            route idRoute
            compile $
                makeItem ("" :: String)
                    >>= loadAndApplyTemplate "templates/blog.html" blogContext
                    >>= loadAndApplyTemplate "templates/default.html" blogContext
                    >>= relativizeUrls

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

compilePage :: PageMap -> PageSpec -> MappingSet -> Variant -> Compiler (Item String)
compilePage pagesByGroup page mappings variant = do
    body <- loadBody (fromFilePath $ pageSource page)
    links <- unsafeCompiler $ readFile "assets/links.md"
    rendered <- renderMarkdown page body links
    let renderedForTarget = Item (fromFilePath $ outputFor page variant) (itemBody rendered)
    transformed <- applyTemplates pagesByGroup page variant mappings renderedForTarget
    saveSnapshotWhenBlog page transformed

renderMarkdown :: PageSpec -> String -> String -> Compiler (Item String)
renderMarkdown page body links = do
    let sourceItem = Item (fromFilePath $ pageSource page) (stripLegacyControls body <> "\n\n" <> links)
    rendered <- writePandocWith writerOptions <$> readPandocWith readerOptions sourceItem
    pure $ Item (fromFilePath $ pageOutput page) (itemBody rendered)

applyTemplates :: PageMap -> PageSpec -> Variant -> MappingSet -> Item String -> Compiler (Item String)
applyTemplates pagesByGroup page variant mappings item = do
    metadata <- getMetadata (fromFilePath $ pageSource page)
    let ctx = pageContext pagesByGroup page variant mappings metadata
        item' = fmap (transformHtml page mappings variant) item
    if isBlogPage page
        then
            loadAndApplyTemplate "templates/post.html" ctx item'
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls
        else
            loadAndApplyTemplate "templates/default.html" ctx item'
                >>= relativizeUrls

pageContext :: PageMap -> PageSpec -> Variant -> MappingSet -> Metadata -> Context String
pageContext pagesByGroup page variant mappings metadata =
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
        <> constField "switcher" (switcherHtml pagesByGroup page variant)
        <> constField "scripts" (scriptsFor page mappings variant)
        <> defaultContext
  where
    transform = transformFor page mappings variant
    title = transform $ fromMaybe "" $ lookupString "title" metadata
    description = transform <$> lookupString "description" metadata
    image = fromMaybe "assets/img/portrait-small.jpg" $ lookupString "image" metadata
    imageAlt = transform <$> lookupString "image-alt" metadata
    titleMeta = transform <$> lookupString "title-meta" metadata

saveSnapshotWhenBlog :: PageSpec -> Item String -> Compiler (Item String)
saveSnapshotWhenBlog page item =
    if isBlogPage page && pageOutput page == outputFor page (defaultVariant page)
        then saveSnapshot "content" item
        else pure item

readerOptions :: ReaderOptions
readerOptions =
    defaultHakyllReaderOptions
        { readerExtensions =
            disableExtension Ext_implicit_figures $
                enableExtension Ext_bracketed_spans $
                    enableExtension Ext_fenced_divs githubMarkdownExtensions
        }

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions

switcherHtml :: PageMap -> PageSpec -> Variant -> String
switcherHtml pagesByGroup page variant =
    "<p><span lang=\"en\">Languages:</span> "
        <> intercalate " · " (languageLinks pagesByGroup page)
        <> "</p>"
        <> if length (pageVariants page) > 1
            then "<p><span lang=\"en\">Scripts:</span> " <> intercalate " · " (variantLinks page variant) <> "</p>"
            else ""

languageLinks :: PageMap -> PageSpec -> [String]
languageLinks pagesByGroup page =
    [ linkFor other (pageLanguageLabel other)
    | other <- M.findWithDefault [page] (pageGroup page) pagesByGroup
    ]
  where
    linkFor other label =
        if pageOutput other == pageOutput page
            then "<strong>" <> label <> "</strong>"
            else "<a href=\"/" <> pageOutput other <> "\">" <> label <> "</a>"

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
