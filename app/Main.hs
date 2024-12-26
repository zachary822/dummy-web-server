{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Foldable (sequenceA_, traverse_)
import Data.Functor
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes)
import Data.String
import Data.Text.Lazy qualified as TL
import Lib.Types
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Options.Applicative
import Web.Scotty

data Config = Config
  { port :: Int
  , configPath :: FilePath
  , origins :: [ByteString]
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

main :: IO ()
main = do
  Config{..} <-
    execParser $
      info (configParser <**> helper) (fullDesc <> progDesc "Dummy web server")

  pc :: Paths <-
    eitherDecodeFileStrict configPath >>= either fail return

  let corsOrigins = if null origins then Nothing else Just (origins, True)

  scotty port $ do
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
