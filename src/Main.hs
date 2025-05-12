module Main where

import qualified GHC.IO.Encoding as E
import Hakyll.Core.Configuration (Configuration(..), defaultConfiguration)
import Hakyll.Main (hakyllWith)

import Site (site)

-- setLocaleEncoding allows Hakyll to support arbitrary Unicode (instead of just ASCII)
main :: IO ()
main = E.setLocaleEncoding E.utf8 >> hakyllWith hakyllConfig site

hakyllConfig :: Configuration
hakyllConfig =
  defaultConfiguration
    { destinationDirectory = "site",
      providerDirectory = "content"
    }