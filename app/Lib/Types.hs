{-# LANGUAGE OverloadedStrings #-}

module Lib.Types where

import Data.Aeson
import GHC.Generics (Generic)
import Lib.OpenApi.Types
import Lib.Path.Types

data ServerConfig = PathConf Paths | OpenApiConf OpenApi
  deriving (Generic, Show, Eq)

instance FromJSON ServerConfig where
  parseJSON = genericParseJSON defaultOptions{sumEncoding = UntaggedValue}
