
module Site.Common where

import qualified System.FilePath as FPL -- FilePath Local, for files on the current machine
import qualified System.FilePath.Posix as FPP -- always use '/' to make site routes
import Text.Read (readMaybe)

import Hakyll

-- | kebab-case id for a section of the site (e.g. "theory").
type SectionId = String

-- | kebab-case id for a page (e.g. "msc"); the identifier for the file
--   containing a page's content is always 'pages/$section-id$/$page-id$.md'
type PageId = String

-- | kebab-case "canonical" fragment name, which should always exist and be targetable
--   by URL fragments, e.g. '/theory/msc.html#self-determination'
type FragmentId = String

-- | Arbitrary text that can serve as a hyperlink reference, e.g. `Metaphysics`_
--   (There may be multiple ways to refer to the same section, depending on context)
type LinkText = String

data CompiledPage m = CompiledPage
  { compiledPageMetadata  :: m,           -- metadata associated with this page
    compiledPageHTML      :: Item String, -- HTML of compiled page, ready to be dropped into templates
    compiledPageTOC       :: String       -- HTML table of contents (i.e. links to headings)
  }

-- 'blah/section-id/page.foo' -> 'section-id'
identifierSectionId :: MonadFail m => Identifier -> m SectionId
identifierSectionId itemId = case FPL.splitDirectories (toFilePath itemId) of
    [_, pid, _] -> return pid
    _           -> fail $ "could not determine section id for " ++ show itemId

filenameOnlyRoute, htmlExtensionRoute, tailRoute, tailHTMLRoute :: Routes
filenameOnlyRoute = customRoute (FPL.takeFileName . toFilePath)
htmlExtensionRoute = setExtension "html"
tailRoute = customRoute (FPP.joinPath . tail . FPL.splitPath . toFilePath)
tailHTMLRoute = composeRoutes tailRoute htmlExtensionRoute

makeItemWith :: (a -> Identifier) -> a -> Item a
makeItemWith makeId body = Item (makeId body) body

makeSubItemWith :: (a -> String) -> (a -> b) -> Item a -> Item b
makeSubItemWith mapIdSuffix mapBody (Item parentId parentBody) = Item newId newBody
  where
    newId = addIdSuffix parentId (mapIdSuffix parentBody)
    newBody = mapBody parentBody

addIdSuffix :: Identifier -> String -> Identifier
addIdSuffix parentId suffix = fromFilePath (toFilePath parentId ++ "_" ++ suffix)

maybeField :: String -> (Item a -> Compiler (Maybe String)) -> Context a
maybeField key mkVal = Context $ \k _ i ->
  if k == key
    then mkVal i >>= maybe (noResult $ "Nothing value for " ++ key) (return . StringField)
    else noResult $ "Tried maybeField " ++ key

readMetadataField :: (MonadFail m, MonadMetadata m, Read a) => Identifier -> String -> m (Maybe a)
readMetadataField ident fieldName = do
  stringVal <- getMetadataField ident fieldName
  return $ stringVal >>= readMaybe

readMetadataField' :: (MonadFail m, MonadMetadata m, Read a) => Identifier -> String -> m a
readMetadataField' ident fieldName = do
  stringVal <- getMetadataField' ident fieldName
  case readMaybe stringVal of
    Just v -> return v
    Nothing -> fail $ "failed to read value " ++ fieldName ++ " from " ++ show ident

dbg :: Show a => a -> Compiler a
dbg x = debugCompiler (show x) >> return x
