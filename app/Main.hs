{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Foldable (sequenceA_, traverse_)
import Data.List.NonEmpty qualified as N
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Scientific
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector qualified as V
import Data.Yaml qualified as Y
import Lib.Common.Types
import Lib.OpenApi.Types as O
import Lib.Path.Types
import Lib.Swagger
import Lib.Types
import Network.HTTP.Types (StdMethod (HEAD, TRACE))
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Options.Applicative
import System.FilePath
import System.Random.Stateful
import Text.Blaze.Html.Renderer.Utf8
import Text.Regex.TDFA
import Web.Scotty as S
import Web.Scotty.Internal.Types (RoutePattern (Capture))

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

parseConfig :: FilePath -> IO (ServerConfig, String)
parseConfig p = do
  unless (ext `elem` [".json", ".yaml", ".yml"]) $ fail "bad config file"

  if ext == ".json"
    then
      eitherDecodeFileStrict p
        >>= either fail (return . (,ext))
    else
      Y.decodeFileEither p
        >>= either (fail . show) (return . (,ext))
 where
  ext = takeExtension p

main :: IO ()
main = do
  conf@Config{..} <-
    execParser $
      info (configParser <**> helper) (fullDesc <> progDesc "Dummy web server")

  let corsOrigins = if nullOf origins conf then Nothing else Just (_origins, True)

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

  parseConfig _configPath
    >>= \case
      (OpenApiConf api@OpenApi{..}, ext) -> do
        let Components{..} = api ^. components
        unless
          (_openapi < Version (3, 2, 0) && _openapi >= Version (3, 0, 0))
          (fail "unsupported openapi version")

        scotty _port $ do
          mws

          if ext == ".json"
            then do
              get "/openapi.json" $ do
                setHeader "Content-Type" "application/json"
                file _configPath
              get "/docs" $ do
                setHeader "Content-Type" "text/html"
                raw $ renderHtml (swaggerPage "/openapi.json")
            else do
              get "/openapi.yaml" $ do
                setHeader "Content-Type" "application/x-yaml"
                file _configPath
              get "/docs" $ do
                setHeader "Content-Type" "text/html"
                raw $ renderHtml (swaggerPage "/openapi.yaml")

          forM_ (api ^. paths . to M.toList & traverse . _2 %~ pathLookup pathItems) $
            \case
              (path, Just (PathItem{..})) -> do
                let prepareOp verb o =
                      flip (maybe mempty) o $
                        \(OperationObject resp) ->
                          forM_ (resp ^. to M.toList & traverse . _2 %~ responseLookup responses) $
                            \case
                              (sts, Just (Response{..})) ->
                                forM_ (M.toList content) $ \(mt, MediaTypeObject schema) ->
                                  verb (Capture (sanitizePath path)) $ do
                                    status (getStatus sts)
                                    setHeader "Content-Type" (TL.fromStrict mt)
                                    case (schemaLookup schemas schema) of
                                      Just s -> do
                                        liftIO (jsonFromSchema s) >>= json
                                      Nothing -> fail "invalid schema"
                              _ -> mempty

                prepareOp get getOp
                prepareOp put putOp
                prepareOp post postOp
                prepareOp delete deleteOp
                prepareOp options optionsOp
                prepareOp patch patchOp
                prepareOp (addroute TRACE) traceOp
                flip (maybe mempty) headOp $
                  \(OperationObject resp) ->
                    forM_ (resp ^. to M.toList & traverse . _2 %~ responseLookup responses) $
                      \case
                        (sts, Just (Response{..})) ->
                          forM_ (M.toList content) $ \(mt, _) ->
                            (addroute HEAD) (Capture (sanitizePath path)) $ do
                              status (getStatus sts)
                              setHeader "Content-Type" (TL.fromStrict mt)
                        _ -> mempty
              _ -> mempty
      (PathConf ps, _) -> do
        scotty _port $ do
          mws

          forM_ (M.toList ps) $ \(path, PathConfig{..}) -> do
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

sanitizePath :: Text -> Text
sanitizePath path = foldr (\m r -> T.replace m ((T.cons ':' . T.tail . T.init) m) r) path matches
 where
  matches = (getAllTextMatches (path =~ ("\\{[^}]+\\}" :: Text)) :: [Text])

chars :: [Char]
chars = [' '] ++ ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']

genItems :: (MonadIO m) => [a] -> Int -> m ([a])
genItems cs n = replicateM n ((cs !!) <$> uniformRM (0, length cs - 1) globalStdGen)

genString :: (MonadIO m) => m Value
genString = String . T.pack <$> (uniformRM (1, 20) globalStdGen >>= genItems chars)

jsonFromSchema :: SchemaObject -> IO Value
jsonFromSchema (SchemaRef _) = fail "invalid reference"
jsonFromSchema (SchemaObject props) =
  fromMaybe (Object mempty)
    <$> traverse
      ( fmap (object . M.foldlWithKey (\a k v -> (fromText k, v) : a) [])
          . traverse jsonFromSchema
      )
      props
jsonFromSchema (SchemaArray item) =
  maybe (Array mempty) (Array . V.fromList) <$> ele
 where
  ele =
    uniformRM (1, 10) globalStdGen
      >>= \n -> fmap sequenceA . replicateM n $ traverse jsonFromSchema item
jsonFromSchema SchemaString = genString
jsonFromSchema SchemaNull = return Null
jsonFromSchema SchemaNumber = Number . fromFloatDigits <$> uniformDouble01M globalStdGen
jsonFromSchema SchemaInteger = Number . (fromIntegral @Int) <$> uniformM globalStdGen
jsonFromSchema SchemaBoolean = Bool <$> uniformM globalStdGen
jsonFromSchema (SchemaAnyOf schemas) = jsonFromSchema (N.head schemas)
jsonFromSchema (SchemaOneOf schemas) = jsonFromSchema (N.head schemas)
jsonFromSchema (SchemaAllOf schemas) = jsonFromSchema (N.head schemas)
jsonFromSchema (SchemaNot SchemaNull) = genString
jsonFromSchema (SchemaNot _) = return Null
