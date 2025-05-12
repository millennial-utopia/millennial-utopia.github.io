module Site.Home (compileHome) where

import Hakyll

import Site.Pandoc

-- Sections have blurbs defined in pages/home
compileHome :: Rules ()
compileHome = match "pages/home.md" $ do
  route $ constRoute "index.html"
  compile $ getResourceBody
    >>= loadAndApplyTemplate "templates/refs.md" defaultContext
    >>= compilePandocMarkdown
    >>= compileHTMLPandoc
    >>= makeItem
    >>= loadAndApplyTemplate "templates/base.html" defaultContext
    >>= relativizeUrls
