module Site.Terms (termContext) where

import Hakyll

import Site.Common
import Site.Config
import Site.Pandoc

-- | Term definition is compiled as markdown with the Utopia template. Term alternatives
-- (plurals, etc) are compiled into a list field, which is used in the terms JS to support
-- pinning/hovering over terms in the article body.
termContext :: Context Term
termContext = mconcat [termField, defnField, altsField]
  where
    termField = field "term" (return . term_term . itemBody)

    defnField = field "definition" compileDefinitionMD

    compileDefinitionMD :: Item Term -> Compiler String
    compileDefinitionMD term = do
      termDefPandoc <- preprocMD >>= compilePandocMarkdown
      compileHTMLPandoc termDefPandoc
      where
        preprocMD =
          loadAndApplyTemplate "templates/millennial-utopia/base.md" defaultContext (term_definition <$> term)

    altsField = listFieldWith "alternatives" altContext altItems

    altItems termItem = return $ makeAltItem termItem <$> term_alternatives (itemBody termItem)
    makeAltItem termItem termAlt = makeSubItemWith (const termAlt) (const altTuple) termItem
      where
        altTuple = (term_term $ itemBody termItem, termAlt)

    altContext = mconcat [altField, altTermField]
    altField = field "alternative" (return . snd . itemBody)
    altTermField = field "term" (return . fst . itemBody)
