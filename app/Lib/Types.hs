{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Types where

import Data.Aeson
import Data.Map.Strict (Map)
import Data.Scientific
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Read (decimal)
import GHC.Generics
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status

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

newtype HTTPStatus = HTTPStatus {unStatus :: Status} deriving (Show, Eq, Ord)

instance FromJSON HTTPStatus where
  parseJSON = withScientific "Status" $ \s ->
    case toBoundedInteger s of
      Nothing -> fail "Bad status code"
      Just i -> return (HTTPStatus . toEnum $ i)

instance FromJSONKey HTTPStatus where
  fromJSONKey = FromJSONKeyTextParser $ \s ->
    case decimal s of
      Left e -> fail e
      Right (i, _) -> return (HTTPStatus . toEnum $ i)

newtype HTTPMethod = HTTPMethod {unMethod :: StdMethod} deriving (Show, Eq, Ord)

instance FromJSON HTTPMethod where
  parseJSON = withText "Method" $ \s ->
    case (parseMethod . T.encodeUtf8 . T.toUpper) s of
      Left e -> fail $ (T.unpack . T.decodeUtf8) e
      Right m -> return $ HTTPMethod m

instance FromJSONKey HTTPMethod where
  fromJSONKey = FromJSONKeyTextParser $ \s ->
    case (parseMethod . T.encodeUtf8 . T.toUpper) s of
      Left e -> fail $ (T.unpack . T.decodeUtf8) e
      Right m -> return $ HTTPMethod m
