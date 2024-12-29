{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (Pair, listValue)
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Foldable (sequenceA_, traverse_)
import Data.Functor
import Data.List.NonEmpty as N
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Lib.OpenApi.Types as O
import Lib.Swagger
import Lib.Types
import Network.HTTP.Types (StdMethod (HEAD, TRACE))
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Options.Applicative
import Text.Blaze.Html.Renderer.Utf8
import Text.Regex.TDFA
import Web.Scotty as S
import Web.Scotty.Internal.Types (RoutePattern (Capture))

data Config = Config
  { port :: Int
  , configPath :: FilePath
  , origins :: [ByteString]
  , openApi :: Bool
  }
  deriving (Show, Eq)

configParser :: Parser Config
configParser =
  Config
    <$> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> showDefault
          <> value 3000
          <> help "listening port"
      )
    <*> strOption
      ( long "config"
          <> short 'c'
          <> metavar "FILE"
          <> help "config file path"
      )
    <*> ( many $
            strOption
              ( long "allowed-origin"
                  <> short 'a'
                  <> help "allowed CORS origins"
              )
        )
    <*> switch (long "openapi" <> help "config is openapi format")

main :: IO ()
main = do
  Config{..} <-
    execParser $
      info (configParser <**> helper) (fullDesc <> progDesc "Dummy web server")

  let corsOrigins = if null origins then Nothing else Just (origins, True)

  let mws = do
        middleware logStdoutDev
        middleware $
          cors $
            const $
              Just
                simpleCorsResourcePolicy
                  { corsRequestHeaders = ["Authorization"]
                  , corsMethods = ["GET", "POST", "PUT", "HEAD", "DELETE", "OPTIONS"]
                  , corsOrigins = corsOrigins
                  }

  if openApi
    then do
      OpenApi{..} <-
        eitherDecodeFileStrict configPath >>= either fail return
      let Components{..} = components
      unless
        (openapi < Version (3, 2, 0) && openapi >= Version (3, 1, 0))
        (fail "unsupported openapi version")

      scotty port $ do
        mws

        get "/openapi.json" $ do
          setHeader "Content-Type" "application/json"
          file configPath
        get "/docs" $ do
          setHeader "Content-Type" "text/html"
          raw $ renderHtml (swaggerPage "/openapi.json")

        forM_ (M.toList paths) $
          ( \case
              (path, Just (PathItem{..})) -> do
                let prepareOp verb op =
                      flip (maybe mempty) op $
                        \(OperationObject resp) ->
                          forM_ (M.toList resp) $
                            ( \case
                                (sts, Just (Response{..})) ->
                                  forM_ (M.toList content) $ \(mt, MediaTypeObject schema) ->
                                    verb (Capture (sanitizePath path)) $ do
                                      status (getStatus sts)
                                      setHeader "Content-Type" (TL.fromStrict mt)
                                      case (schemaLookup schemas schema) of
                                        Just s -> do
                                          jsonFromSchema s >>= json
                                        Nothing -> mempty
                                (_, Nothing) -> mempty
                            )
                              . (second (responseLookup responses))
                prepareOp get getOp
                prepareOp put putOp
                prepareOp post postOp
                prepareOp delete deleteOp
                prepareOp options optionsOp
                prepareOp patch patchOp
                prepareOp (addroute TRACE) traceOp
                flip (maybe mempty) headOp $
                  \(OperationObject resp) ->
                    forM_ (M.toList resp) $
                      ( \case
                          (sts, Just (Response{..})) ->
                            forM_ (M.toList content) $ \(mt, _) ->
                              (addroute HEAD) (Capture (sanitizePath path)) $ do
                                status (getStatus sts)
                                setHeader "Content-Type" (TL.fromStrict mt)
                          (_, Nothing) -> mempty
                      )
                        . (second (responseLookup responses))
              (_, Nothing) -> mempty
          )
            . (second (pathLookup pathItems))
    else do
      pc :: Paths <-
        eitherDecodeFileStrict configPath >>= either fail return

      scotty port $ do
        mws

        forM_ (M.toList pc) $ \(path, PathConfig{..}) -> do
          let addroute' = maybe matchAny (addroute . unMethod) responseMethod
          addroute' (fromString path) $ do
            sequenceA_ $
              catMaybes
                [ liftIO . threadDelay <$> responseDelay
                , status . unStatus <$> responseStatus
                , traverse_ (uncurry addHeader) . M.toList <$> responseHeaders
                , responseBody <&> \case
                    String t -> text (TL.fromStrict t)
                    v -> json v
                ]
 where

sanitizePath :: Text -> Text
sanitizePath path = foldr (\m r -> T.replace m ((T.cons ':' . T.tail . T.init) m) r) path matches
 where
  matches = (getAllTextMatches (path =~ ("\\{[^}]+\\}" :: Text)) :: [Text])

jsonFromSchema :: (MonadFail m) => SchemaObject -> m Value
jsonFromSchema (SchemaRef _) = fail "invalid reference"
jsonFromSchema (SchemaObject props) =
  (maybe (object []) object <$>) . sequenceA $
    sequenceA
      . M.foldlWithKey
        (\a k v -> (((fromText k .=) :: Value -> Pair) <$> jsonFromSchema v) : a)
        []
      <$> props
jsonFromSchema (SchemaArray item) =
  (maybe (Array mempty) (listValue id . (: [])) <$>) . sequenceA $
    jsonFromSchema <$> item
jsonFromSchema SchemaString = return $ String "yay"
jsonFromSchema SchemaNull = return Null
jsonFromSchema SchemaNumber = return (Number 1.0)
jsonFromSchema SchemaInteger = return (Number 1)
jsonFromSchema SchemaBoolean = return (Bool True)
jsonFromSchema (SchemaAnyOf schemas) = jsonFromSchema (N.head schemas)
jsonFromSchema (SchemaOneOf schemas) = jsonFromSchema (N.head schemas)
jsonFromSchema (SchemaAllOf schemas) = jsonFromSchema (N.head schemas)
jsonFromSchema (SchemaNot SchemaNull) = return (String "yay")
jsonFromSchema (SchemaNot _) = return Null
