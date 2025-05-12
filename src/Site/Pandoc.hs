-- | Functions ending with ' use the underlying resource; others use the `Item` passed to the function
module Site.Pandoc where

import qualified Data.Text as T
import Data.Text (Text)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Pandoc
import Text.Pandoc.Shared (inlineListToIdentifier, stringify)
import Text.Pandoc.Writers.Shared (toTableOfContents)

import Hakyll
import Site.Common
import Site.Page

-- | Apply the base markdown template then parse it through Pandoc, using default reader options
compilePandocMarkdown :: Item String -> Compiler (Item Pandoc)
compilePandocMarkdown = compilePandocMarkdownWith myDefaultReaderOptions

-- | Apply the base markdown template then parse it through Pandoc, using default reader options
compilePandocMarkdown' :: Compiler (Item Pandoc)
compilePandocMarkdown' = compilePandocMarkdownWith' myDefaultReaderOptions

-- | Apply the base markdown template to the current resource then parse it through Pandoc, using given reader options
compilePandocMarkdownWith' :: ReaderOptions -> Compiler (Item Pandoc)
compilePandocMarkdownWith' ropt = getResourceBody >>= compilePandocMarkdownWith ropt

-- | Apply the base markdown template then parse it through Pandoc, using given reader options
compilePandocMarkdownWith :: ReaderOptions -> Item String -> Compiler (Item Pandoc)
compilePandocMarkdownWith ropt mdItem = do
  parsedPandoc <- compilePandocPure $ readMarkdown ropt (T.pack . itemBody $ mdItem)
  makeItem parsedPandoc

-- | Compile a Pandoc document to HTML, using default writer options
compileHTMLPandoc :: Item Pandoc -> Compiler String
compileHTMLPandoc = compileHTMLPandocWith myDefaultWriterOptions

-- | Compile a Pandoc document to HTML, using given writer options
compileHTMLPandocWith :: WriterOptions -> Item Pandoc -> Compiler String
compileHTMLPandocWith wopt docItem = compilePandocPure $ T.unpack <$> writeHtml5String wopt (itemBody docItem)

-- | Compile a Pandoc document using default writer options, with attached page metadata for templating
compilePandocPage :: PageMetadata m => Item Pandoc -> Compiler (CompiledPage m)
compilePandocPage = compilePandocPageWith myDefaultWriterOptions

-- | Compile a Pandoc document using given writer options, with attached page metadata for templating
compilePandocPageWith :: PageMetadata m => WriterOptions -> Item Pandoc -> Compiler (CompiledPage m)
compilePandocPageWith wopt docItem = do
  md <- compilePageMetadata (itemIdentifier docItem)
  (toc, htmlBody) <- compilePandocPure runPandoc
  bodyItem <- makeItem htmlBody
  return $ CompiledPage md bodyItem toc
  where
    doc = itemBody docItem

    runPandoc = do
      toc <- pandocContents doc
      htmlBody <- writeHtml5 wopt (addAnchorLinks doc)
      return (T.unpack toc, renderHtml htmlBody)

    pandocContents (Pandoc meta blocks) = writeHtml5String wopt (Pandoc meta [toTableOfContents wopt blocks])

myDefaultReaderOptions :: ReaderOptions
myDefaultReaderOptions = defaultHakyllReaderOptions
  { readerExtensions = enableExtension Ext_smart (readerExtensions defaultHakyllReaderOptions)
  }

myDefaultWriterOptions :: WriterOptions
myDefaultWriterOptions = defaultHakyllWriterOptions
  { writerWrapText = WrapNone
  , writerHTMLMathMethod = MathJax ""
  }

compilePandocPure :: PandocPure a -> Compiler a
compilePandocPure p = either (fail . pandocErrorMsg) pandocLogger (runPure $ pair getLog p)
  where
    pandocErrorMsg err = "Site.Pandoc.compilePandocPure: pandoc failed: " ++ show err

    pandocLogger (logs, out) = do
      mapM_ (debugCompiler . T.unpack . showLogMessage) logs
      return out

pair :: Applicative m => m a -> m b -> m (a, b)
pair x y = (,) <$> x <*> y

-- | Adds anchor links (which appear as clickable "link" icons) to section headers
addAnchorLinks :: Pandoc -> Pandoc
addAnchorLinks (Pandoc m bs) = Pandoc m (addInlineAnchor <$> bs)
  where
    addInlineAnchor :: Block -> Block
    addInlineAnchor (Header l a ils) = Header l a (ils ++ [anchorLink ils])
    addInlineAnchor (Div a divBs) = Div a (addInlineAnchor <$> divBs)
    addInlineAnchor b = b

    anchorLink :: [Inline] -> Inline
    anchorLink hInlines = Link attr [Str "anchor"] ("#" <> anchorId hInlines, anchorTitle hInlines)
      where attr = ("", ["material-symbols-outlined", "anchor"], [])

    anchorId :: [Inline] -> Text
    anchorId = inlineListToIdentifier emptyExtensions

    anchorTitle :: [Inline] -> Text
    anchorTitle = stringify
