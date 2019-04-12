{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import           Data.Aeson (ToJSON)
import           Data.Function
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           GHC.Generics (Generic)
import           Network.Wai.Handler.Warp (run)
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan ()
import           UsernamePassword

-- Known issues: JWT/Cookie are both required despite not being used: https://github.com/haskell-servant/servant-auth/pull/120

type API = Auth '[UsernamePassword] User :> Protected

data User =
  User
    { name :: Text
    , password :: Text
    }
  deriving (Show, Generic)

instance ToJWT User
instance ToJSON User
instance FromHeaders User where fromHeaders = on User T.decodeUtf8

type Protected
   = "username" :> Get '[JSON] String
 :<|> "password" :> Get '[JSON] String

protected :: AuthResult User -> Server Protected
protected (Authenticated user) =
  return (show (name user)) :<|> return (show (password user))
protected _ = throwAll err401

main :: IO ()
main = do
  dummy <- generateKey
  let usernamePasswordSettings =
        UsernamePasswordSettings
          { usernamePasswordSettingsUsername = "chris"
          , usernamePasswordSettingsPassword = "secret"
          }
      config =
        defaultJWTSettings dummy :. usernamePasswordSettings :.
        defaultCookieSettings :.
        EmptyContext
  run 7249 (serveWithContext (Proxy :: Proxy API) config protected)
