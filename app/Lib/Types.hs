{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Types where

import Control.Lens
import Data.Aeson
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Lib.OpenApi.Types
import Lib.Path.Types

data ServerConfig = PathConf Paths | OpenApiConf OpenApi
  deriving (Generic, Show, Eq)

instance FromJSON ServerConfig where
  parseJSON = genericParseJSON defaultOptions{sumEncoding = UntaggedValue}

data Config = Config
  { _port :: !Int
  , _configPath :: !FilePath
  , _origins :: ![ByteString]
  }
  deriving (Show, Eq)

makeLenses ''Config
