{-# LANGUAGE OverloadedStrings #-}

module Lib.Swagger where

import Data.String (IsString (fromString))
import Data.Text
import Text.Blaze
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Printf

crossorigin :: Attribute
crossorigin = customAttribute "crossorigin" mempty

swaggerPage :: Text -> Html
swaggerPage url = do
  docType
  html ! lang "en" $ do
    H.head $ do
      meta ! charset "utf-8"
      meta ! name "viewport" ! content "width=device-width, initial-scale=1"
      meta ! name "description" ! content "SwaggerUI"
      H.title "SwaggerUI"
      link
        ! rel "stylesheet"
        ! href "https://unpkg.com/swagger-ui-dist@5.11.0/swagger-ui.css"
    body $ do
      H.div ! A.id "swagger-ui" $ mempty
      script
        ! src "https://unpkg.com/swagger-ui-dist@5.11.0/swagger-ui-bundle.js"
        ! crossorigin
        $ mempty
      script $
        fromString
          ( printf
              "window.onload = () => { window.ui = SwaggerUIBundle({ url: '%s', dom_id: '#swagger-ui' }); };"
              url
          )
