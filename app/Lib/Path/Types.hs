{-# LANGUAGE OverloadedStrings #-}

module Lib.Path.Types where

import Data.Aeson
import Data.Map.Strict (Map)
import Data.Text.Lazy qualified as TL
import GHC.Generics
import Lib.Common.Types

type Paths = Map String PathConfig

data PathConfig = PathConfig
  { responseMethod :: !(Maybe HTTPMethod)
  , responseStatus :: !(Maybe HTTPStatus)
  , responseDelay :: !(Maybe Int)
  , responseBody :: !(Maybe Value)
  , responseHeaders :: !(Maybe (Map TL.Text TL.Text))
  }
  deriving (Generic, Show, Eq)

instance FromJSON PathConfig where
  parseJSON = withObject "PathConfig" $ \o ->
    PathConfig
      <$> o .:? "method"
      <*> o .:? "status"
      <*> o .:? "delay"
      <*> o .:? "body"
      <*> o .:? "headers"
