
module Site.Lemontopia (compileLemontopia) where

import Data.Ord (comparing)

import Hakyll
import Site.Common
import Site.Page
import Site.Pandoc
import Site.Section

lemontopiaSectionId :: SectionId
lemontopiaSectionId = "lemontopia"

data LemontopiaPageMetadata = LemontopiaPageMetadata
  { lemontopiaPageId    :: PageId
  , lemontopiaPageTitle :: String
  , lemontopiaTimelineYear :: Int
  , lemontopiaPageBlurb :: String
  } deriving (Eq)

instance PageMetadata LemontopiaPageMetadata where
  pageId = lemontopiaPageId
  sectionId = const lemontopiaSectionId
  pageTitle = lemontopiaPageTitle
  pageDescription = lemontopiaPageBlurb

  compilePageMetadata pg = do
    title <- getMetadataField' pg "title"
    blurb <- getMetadataField' pg "blurb"
    year  <- readMetadataField' pg "timeline-year"
    return $ LemontopiaPageMetadata (identifierPageId pg) title year blurb

instance Ord LemontopiaPageMetadata where
  compare = comparing lemontopiaTimelineYear

type CompiledLemontopiaPage = CompiledPage LemontopiaPageMetadata
type LemontopiaPageCompiler = Compiler CompiledLemontopiaPage

compileLemontopia :: Rules ()
compileLemontopia = do
  compilePages
  compileIndex

compilePages, compileIndex :: Rules ()

compilePages = matchSectionPages lemontopiaSectionId $ do
  route tailHTMLRoute
  compile $ lemontopiaPageCompiler >>= applyLemontopiaTemplates

compileIndex = matchSectionIndex lemontopiaSectionId $ do
  route tailHTMLRoute
  compile $ getResourceBody
    >>= loadAndApplyTemplate "templates/refs.md" defaultContext
    >>= compilePandocMarkdown
    >>= compileHTMLPandoc
    >>= makeItem
    >>= applyIndexTemplates

applyIndexTemplates :: Item String -> Compiler (Item String)
applyIndexTemplates pageBodyHTMLItem = do
  loadAndApplyTemplate "templates/lemontopia-index.html" (tocContext <> defaultContext) pageBodyHTMLItem
    >>= loadAndApplyTemplate "templates/base.html" baseContext
    >>= relativizeUrls
  where
    tocContext = sectionContentsContext lemontopiaSectionId (timelineYearContext <> defaultPageMetadataContext)
    timelineYearContext = field "timeline-year" (return . show . lemontopiaTimelineYear . itemBody)
    baseContext  = sectionIdContext <> sectionNameContext <> defaultContext

lemontopiaPageCompiler :: LemontopiaPageCompiler
lemontopiaPageCompiler = getResourceBody
  >>= loadAndApplyTemplate "templates/refs.md" defaultContext
  >>= compilePandocMarkdown
  >>= compilePandocPage

applyLemontopiaTemplates :: CompiledLemontopiaPage -> Compiler (Item String)
applyLemontopiaTemplates (CompiledPage md pageHTMLItem toc) =
  loadAndApplyTemplate "templates/page.html" (pageContext <> baseContext) pageHTMLItem
    >>= loadAndApplyTemplate "templates/base.html" baseContext
    >>= relativizeUrls
    where
      pageContext = constField "toc" toc <> defaultContext
      baseContext = sectionIdContext <> sectionNameContext <> headerTitleContext <> defaultContext
      headerTitleContext = constField "header-title" (pageTitle md)
