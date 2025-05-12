
-- | Functions and types about `Page`s, which have links to and from other `Page`s in the same `Section`
--   Something is a `Page` iff its Identifier looks like `pages/section-id/page-name.md`

module Site.Page where

import qualified System.FilePath as FPL -- FilePath Local, for files on the current machine

import Hakyll
import Site.Common

pageIdentifier :: SectionId -> PageId -> Identifier
pageIdentifier secId pgId = fromFilePath $ FPL.joinPath ["pages", secId, pgId ++ ".md"]

sectionPagesGlob :: SectionId -> Pattern
sectionPagesGlob secId = fromGlob $ FPL.joinPath ["pages", secId, "*.md"]

identifierPageId :: Identifier -> PageId
identifierPageId = FPL.takeBaseName . toFilePath

getUnderlyingBaseName :: Compiler String
getUnderlyingBaseName = identifierBaseName <$> getUnderlying

identifierBaseName :: Identifier -> String
identifierBaseName = FPL.takeBaseName . toFilePath

class PageMetadata m where
  pageId :: m -> String
  sectionId :: m -> String
  pageTitle :: m -> String
  pageDescription :: m ->String

  compilePageMetadata :: Identifier -> Compiler m

currentPageMetadata :: PageMetadata m => Compiler m
currentPageMetadata = getUnderlying >>= compilePageMetadata

defaultPageMetadataContext :: PageMetadata m => Context m
defaultPageMetadataContext =
  field "page-id" (return . pageId . itemBody)
  <> field "page-title" (return . pageTitle . itemBody)
  <> field "blurb" (return . pageDescription . itemBody)