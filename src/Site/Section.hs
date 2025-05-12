
-- | Functions and types about `Section`s, which are collections of `Page`s with independent internal links and their own index
module Site.Section where

import Data.Char (toTitle)
import Data.List (sortOn)

import Hakyll
import Site.Common
import Site.Page

-- | The section id for the file we're currently working on
currentSectionId :: Compiler SectionId
currentSectionId = getUnderlying >>= identifierSectionId

-- | A template context for the section-id of the file currently being compiled
sectionIdContext :: Context a
sectionIdContext = field "section-id" (const currentSectionId)

sectionNameContext :: Context a
sectionNameContext = field "section-name" (const currentSectionName)
  where
    currentSectionName = kebabToTitleCase <$> currentSectionId

    kebabToTitleCase kc = unwords $ titleCaseWord <$> splitKebab kc

    titleCaseWord "" = ""
    titleCaseWord (c:cs) = toTitle c : cs

    splitKebab k = case dropWhile (== '-') k of
      "" -> []
      k' -> w : splitKebab k''
        where
          (w, k'') = break (== '-') k'

-- | A matcher for 'pages/section-id/index.*'
matchSectionIndex :: SectionId -> Rules () -> Rules ()
matchSectionIndex pid = match $ sectionIndexPattern pid

sectionIndexPattern :: SectionId -> Pattern
sectionIndexPattern pid = fromGlob $ "pages/" ++ pid ++ "/index.md"

-- | A matcher for everything in 'pages/section-id/', except for the index
matchSectionPages :: SectionId -> Rules () -> Rules ()
matchSectionPages pid = match $ sectionPagesPattern pid

sectionPagesPattern :: SectionId -> Pattern
sectionPagesPattern pid = sectionPagesGlob pid .&&. notSectionIndex
  where
    notSectionIndex = complement . fromGlob $ "pages/" ++ pid ++ "/index.md"

-- The context argument not only provides a way to pass in extra context (you can also pass mempty), but it also
-- helps the compiler fix the particular page metadata type (since the context type is weird)
sectionContentsContext :: (PageMetadata m, Ord m) => SectionId -> Context m -> Context a
sectionContentsContext secId mdContext = listField "section-contents" (mdContext <> defaultPageMetadataContext) tocMetadataCompiler
  where
    matchingPagesCompiler :: Compiler [Item String]
    matchingPagesCompiler = loadAll (sectionPagesPattern secId)

    tocMetadataCompiler = do
      -- first find all the page identifiers for this section...
      matchingPageIdentifiers <- fmap itemIdentifier <$> matchingPagesCompiler
      -- ...then extract their metadata, according to the specific metadata type...
      pageMetadataItems <- mapM compilePageMetadataItem matchingPageIdentifiers
      -- ...then sort them as defined for the particular section's metadata typ
      return $ sortOn itemBody pageMetadataItems

    compilePageMetadataItem pgIdent = Item (addIdSuffix pgIdent "_PageMetadata") <$> compilePageMetadata pgIdent
