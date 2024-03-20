{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.Aeson
import Data.Map.Strict qualified as M
import Data.String
import Lib.Types
import Network.Wai.Middleware.RequestLogger
import Options.Applicative
import Web.Scotty

data Config = Config
  { port :: Int
  , configPath :: FilePath
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
    <*> argument str (metavar "FILE")

main :: IO ()
main = do
  Config{..} <- execParser $ info (configParser <**> helper) (fullDesc <> progDesc "Dummy web server")

  pc :: Paths <-
    eitherDecodeFileStrict configPath >>= \case
      Left e -> fail e
      Right c -> return c

  scotty port $ do
    middleware logStdoutDev

    forM_ (M.toList pc) $ \(path, PathConfig{..}) -> do
      addroute (unMethod responseMethod) (fromString path) $ do
        status (unStatus responseStatus)

        case responseBody of
          Nothing -> mempty
          Just b -> json b

        case responseHeaders of
          Nothing -> mempty
          Just hs -> forM_ (M.toList hs) (uncurry addHeader)
