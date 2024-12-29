module Lib.Common.Types where

import Data.Aeson
import Data.Scientific
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Read (decimal)
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status

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
