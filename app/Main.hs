{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as M
import Data.String
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
      )
    <*> strOption
      ( long "config"
          <> short 'c'
          <> metavar "FILE"
      )
    <*> ( many $
            strOption
              ( long "allowed-origin"
                  <> short 'a'
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
        case responseStatus of
          Nothing -> mempty
          Just s -> status $ unStatus s

        case responseDelay of
          Nothing -> mempty
          Just d -> liftIO $ threadDelay d

        case responseHeaders of
          Nothing -> mempty
          Just hs -> forM_ (M.toList hs) (uncurry addHeader)

        case responseBody of
          Nothing -> mempty
          Just b -> json b
