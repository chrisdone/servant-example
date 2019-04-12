{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import           GHC.Generics
import           Servant
import           Servant.Auth.Server.SetCookieOrphan ()
import           Servant.Docs

newtype Greet = Greet Text
  deriving (Generic, Show)
instance FromJSON Greet
instance ToJSON Greet

-- | We can also implement 'MimeRender' for additional formats like 'PlainText'.
instance MimeRender PlainText Greet where
    mimeRender Proxy (Greet s) = "\"" <> LT.encodeUtf8 (LT.fromStrict s) <> "\""

instance ToSample Greet where
  toSamples _ =
    [ ("If you use ?page=1", Greet "HELLO, HASKELLER")
    , ("If you use ?page=2", Greet "Hello, haskeller")
    ]

instance ToCapture (Capture "page" Int) where
  toCapture _ = DocCapture "page" "result page"

type MyAPI = Capture "page" Int :> Get '[JSON] Greet

server :: Applicative f => Int -> f Greet
server (_page :: Int) = pure (Greet "some results")

main :: IO ()
main =
  do let api = docs (Proxy :: Proxy MyAPI)
     print api
     writeFile "README.md" (markdown api)
