{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.OpenApi.Types where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Either
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read (decimal)
import GHC.Generics (Generic)
import Lib.Types
import Network.HTTP.Types (Status, ok200)

data OpenApi = OpenApi
  { openapi :: Version
  , paths :: M.Map Text PathItemObject
  , components :: Components
  }
  deriving (Generic, Show, Eq)

instance FromJSON OpenApi where
  parseJSON = withObject "OpenApi" $ \o ->
    OpenApi
      <$> o .: "openapi"
      <*> o .: "paths"
      <*> (maybe mempty id <$> o .:? "components")

newtype Version = Version (Int, Int, Int) deriving (Generic, Show, Eq, Ord)

instance FromJSON Version where
  parseJSON =
    withText "Version" $
      ( \case
          [a, b, c] -> return $ Version (a, b, c)
          _ -> fail "invalid version"
      )
        . map fst
        . rights
        . map decimal
        . T.split (== '.')

data Components = Components
  { schemas :: M.Map Text SchemaObject
  , responses :: M.Map Text ResponseObject
  , pathItems :: M.Map Text PathItemObject
  }
  deriving (Generic, Show, Eq)

instance FromJSON Components where
  parseJSON = withObject "Components" $ \o ->
    Components
      <$> (maybe mempty id <$> o .:? "schemas")
      <*> (maybe mempty id <$> o .:? "responses")
      <*> (maybe mempty id <$> o .:? "pathItems")

instance Semigroup Components where
  Components s r p <> Components s' r' p' = Components (s <> s') (r <> r') (p <> p')

instance Monoid Components where
  mempty = Components mempty mempty mempty

pathLookup :: M.Map Text PathItemObject -> PathItemObject -> Maybe PathItem
pathLookup pathItemContent (PathRef (Ref r)) =
  T.stripPrefix "#/components/pathItems/" r
    >>= (pathItemContent M.!?)
    >>= \case
      ref@(PathRef _) -> pathLookup pathItemContent ref
      PathItemObject item -> return item
pathLookup _ (PathItemObject pathItem) = return pathItem

responseLookup :: M.Map Text ResponseObject -> ResponseObject -> Maybe Response
responseLookup responseContent (ResponseRef (Ref r)) =
  T.stripPrefix "#/components/responses/" r
    >>= (responseContent M.!?)
    >>= \case
      ref@(ResponseRef _) -> responseLookup responseContent ref
      ResponseObject item -> return item
responseLookup _ (ResponseObject item) = return item

schemaLookup :: M.Map Text SchemaObject -> SchemaObject -> Maybe SchemaObject
schemaLookup schemaContent (SchemaRef (Ref r)) =
  T.stripPrefix "#/components/schemas/" r
    >>= (schemaContent M.!?)
    >>= \case
      ref@(SchemaRef _) -> schemaLookup schemaContent ref
      s -> return s
schemaLookup schemaContent (SchemaObject (Just props)) =
  (sequenceA $ M.map (schemaLookup schemaContent) props)
    >>= return
      . SchemaObject
      . return
schemaLookup _ obj@(SchemaObject Nothing) = return obj
schemaLookup schemaContent (SchemaArray (Just items)) = return (SchemaArray (schemaLookup schemaContent items))
schemaLookup _ arr@(SchemaArray Nothing) = return arr
schemaLookup _ SchemaNull = return SchemaNull
schemaLookup _ SchemaString = return SchemaString
schemaLookup _ SchemaNumber = return SchemaNumber
schemaLookup _ SchemaInteger = return SchemaInteger
schemaLookup _ SchemaBoolean = return SchemaBoolean
schemaLookup schemaContent (SchemaAllOf items) = SchemaAllOf <$> traverse (schemaLookup schemaContent) items
schemaLookup schemaContent (SchemaAnyOf items) = SchemaAnyOf <$> traverse (schemaLookup schemaContent) items
schemaLookup schemaContent (SchemaOneOf items) = SchemaOneOf <$> traverse (schemaLookup schemaContent) items
schemaLookup schemaContent (SchemaNot item) = SchemaNot <$> (schemaLookup schemaContent item)

data PathItemObject
  = PathRef Ref
  | PathItemObject PathItem
  deriving (Generic, Show, Eq)

instance FromJSON PathItemObject where
  parseJSON = withObject "PathItem" $ \o ->
    o .:? "$ref" >>= \case
      Just ref -> return $ PathRef (Ref ref)
      Nothing -> PathItemObject <$> parseJSON (Object o)

data PathItem = PathItem
  { getOp :: Maybe OperationObject
  , putOp :: Maybe OperationObject
  , postOp :: Maybe OperationObject
  , deleteOp :: Maybe OperationObject
  , optionsOp :: Maybe OperationObject
  , headOp :: Maybe OperationObject
  , patchOp :: Maybe OperationObject
  , traceOp :: Maybe OperationObject
  }
  deriving (Generic, Show, Eq)

instance FromJSON PathItem where
  parseJSON = withObject "PathItem" $ \o ->
    PathItem
      <$> o .:? "get"
      <*> o .:? "put"
      <*> o .:? "post"
      <*> o .:? "delete"
      <*> o .:? "options"
      <*> o .:? "head"
      <*> o .:? "patch"
      <*> o .:? "trace"

data OperationObject = OperationObject
  {responses :: M.Map ResponseStatus ResponseObject}
  deriving (Generic, Show, Eq)

instance FromJSON OperationObject

data ResponseObject
  = ResponseRef Ref
  | ResponseObject Response
  deriving (Generic, Show, Eq)

instance FromJSON ResponseObject where
  parseJSON = genericParseJSON defaultOptions{sumEncoding = UntaggedValue}

data Response = Response
  { content :: M.Map Text MediaTypeObject
  }
  deriving (Generic, Show, Eq)

instance FromJSON Response

data MediaTypeObject
  = MediaTypeObject
  { schema :: SchemaObject
  }
  deriving (Generic, Show, Eq)

instance FromJSON MediaTypeObject

data SchemaObject
  = SchemaRef Ref
  | SchemaObject
      { properties :: Maybe (M.Map Text SchemaObject)
      }
  | SchemaArray
      { items :: Maybe SchemaObject
      }
  | SchemaNull
  | SchemaString
  | SchemaNumber
  | SchemaInteger
  | SchemaBoolean
  | SchemaAllOf (NonEmpty SchemaObject)
  | SchemaAnyOf (NonEmpty SchemaObject)
  | SchemaOneOf (NonEmpty SchemaObject)
  | SchemaNot SchemaObject
  deriving (Generic, Show, Eq)

instance FromJSON SchemaObject where
  parseJSON = withObject "schema" $ \o ->
    o .:? "$ref" >>= \case
      Just ref -> return (SchemaRef (Ref ref))
      Nothing ->
        (o .:? "type" :: Parser (Maybe Text)) >>= \case
          Just t -> case t of
            "object" -> SchemaObject <$> o .:? "properties"
            "array" -> SchemaArray <$> o .:? "items"
            "null" -> return SchemaNull
            "string" -> return SchemaString
            "number" -> return SchemaNumber
            "integer" -> return SchemaInteger
            "boolean" -> return SchemaBoolean
            _ -> fail "invalid schema"
          Nothing ->
            ( (SchemaAllOf <$> o .: "allOf")
                <|> (SchemaAnyOf <$> o .: "anyOf")
                <|> (SchemaOneOf <$> o .: "oneOf")
                <|> (SchemaNot <$> o .: "not")
            )

data ResponseStatus
  = ResponseDefault
  | ResponseStatus HTTPStatus
  deriving (Generic, Show, Eq, Ord)

instance FromJSON ResponseStatus where
  parseJSON = withText "status" parseHttpStatus

instance FromJSONKey ResponseStatus where
  fromJSONKey = FromJSONKeyTextParser parseHttpStatus

parseHttpStatus :: (MonadFail m) => Text -> m ResponseStatus
parseHttpStatus "default" = return ResponseDefault
parseHttpStatus num = case decimal num of
  Left e -> fail e
  Right (i, _) -> return (ResponseStatus (HTTPStatus . toEnum $ i))

getStatus :: ResponseStatus -> Status
getStatus ResponseDefault = ok200
getStatus (ResponseStatus (HTTPStatus s)) = s

data Ref = Ref
  { ref :: Text
  }
  deriving (Generic, Show, Eq)

instance FromJSON Ref where
  parseJSON = withObject "ref" $ \o ->
    Ref <$> o .: "$ref"
