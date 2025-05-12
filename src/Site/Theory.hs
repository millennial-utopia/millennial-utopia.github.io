
module Site.Theory (compileTheory) where

import Data.Ord (comparing)

import Hakyll
import Site.Common
import Site.Config
import Site.Page
import Site.Pandoc
import Site.Section
import Site.Terms

theorySectionId :: SectionId
theorySectionId = "theory"

data TheoryPageMetadata = TheoryPageMetadata
  { theoryPageId    :: PageId
  , theoryPageTitle :: String
  , theoryPageOrder :: Int
  , theoryPageBlurb :: String
  } deriving (Eq)

instance PageMetadata TheoryPageMetadata where
  pageId = theoryPageId
  sectionId = const theorySectionId
  pageTitle = theoryPageTitle
  pageDescription = theoryPageBlurb

  compilePageMetadata pg = do
    title <- getMetadataField' pg "title"
    blurb <- getMetadataField' pg "blurb"
    order <- readMetadataField' pg "page-order"
    return $ TheoryPageMetadata (identifierPageId pg) title order blurb

instance Ord TheoryPageMetadata where
  compare = comparing theoryPageOrder

type CompiledTheoryPage = CompiledPage TheoryPageMetadata
type TheoryPageCompiler = Compiler CompiledTheoryPage

compileTheory :: Rules ()
compileTheory = do
  compileJs
  compilePages
  compileIndex

compileJs, compilePages, compileIndex :: Rules ()

compileJs = return ()
{- TODO: bring back term definitions, but for the whole site, and without requiring "highlighting" in the markdown
match "js/theory/*.js" $ do
  route idRoute
  compile $ do
    config <- configCompiler
    jsTemplate <- getResourceBody
    applyAsTemplate (termsContext config) jsTemplate
-}

compilePages = matchSectionPages theorySectionId $ do
  route tailHTMLRoute
  compile $ do
    config <- configCompiler
    theoryPageCompiler >>= applyTheoryTemplates config

compileIndex = matchSectionIndex theorySectionId $ do
  route tailHTMLRoute
  compile $ getResourceBody
    >>= loadAndApplyTemplate "templates/refs.md" defaultContext
    >>= compilePandocMarkdown
    >>= compileHTMLPandoc
    >>= makeItem
    >>= applyIndexTemplates

applyIndexTemplates :: Item String -> Compiler (Item String)
applyIndexTemplates pageBodyHTMLItem = do
  loadAndApplyTemplate "templates/theory-index.html" (tocContext <> defaultContext) pageBodyHTMLItem
    >>= loadAndApplyTemplate "templates/base.html" baseContext
    >>= relativizeUrls
  where
    tocContext = sectionContentsContext theorySectionId (defaultPageMetadataContext :: Context TheoryPageMetadata)

    baseContext  = sectionIdContext <> sectionNameContext <> defaultContext

theoryPageCompiler :: TheoryPageCompiler
theoryPageCompiler = getResourceBody
  >>= loadAndApplyTemplate "templates/refs.md" defaultContext
  >>= compilePandocMarkdown
  >>= compilePandocPage

applyTheoryTemplates :: Config -> CompiledTheoryPage -> Compiler (Item String)
applyTheoryTemplates config (CompiledPage md pageHTMLItem toc) =
  loadAndApplyTemplate "templates/page.html" (pageContext <> baseContext) pageHTMLItem
    >>= loadAndApplyTemplate "templates/base.html" baseContext
    >>= relativizeUrls
    where
      pageContext = constField "toc" toc {- TODO <> termsContext config -} <> defaultContext
      baseContext = sectionIdContext <> sectionNameContext <> headerTitleContext <> defaultContext
      headerTitleContext = constField "header-title" (pageTitle md)

-- term list
termsContext :: Config -> Context String
termsContext config = listField "terms" termContext termItems
  where
    termItems = return $ makeTermItem <$> terms config
    makeTermItem = makeItemWith (\(Term term _ _) -> fromFilePath $ "__term_" ++ term)
