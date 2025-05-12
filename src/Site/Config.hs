{-# LANGUAGE TemplateHaskell #-}

module Site.Config where

import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (FromJSON, Options (fieldLabelModifier))
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Yaml.Aeson (ParseException, decodeEither')
import Hakyll.Core.Compiler (Compiler, loadBody)

{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}

data Term = Term
  { term_term :: String,
    term_alternatives :: [String],
    term_definition :: String
  }

$(deriveJSON (defaultOptions {fieldLabelModifier = drop 5}) ''Term)

-- | Placeholder for a config file which may be desirable in the future
data Config = Config
  { terms :: [Term]
  }

$(deriveJSON defaultOptions ''Config)

-- Note: Don't forget to load the config file in Hakyll rules before using this!
configCompiler :: Compiler Config
configCompiler = loadBody "config.yaml" >>= yamlCompiler
  where
    yamlCompiler bs = either handleYamlError return (decodeEither' bs)

    -- Note: needs FlexibleContexts to compile without a type signature!
    handleYamlError :: FromJSON a => ParseException -> Compiler a
    handleYamlError pe = throwError [show pe]
