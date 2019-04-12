{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

-- | A username and password header authentication.

module UsernamePassword
  ( UsernamePasswordSettings(..)
  , UsernamePassword
  , FromHeaders(..)
  ) where

import Data.ByteString (ByteString)
import Network.Wai
import Servant.Auth.Server.Internal.Class
import Servant.Auth.Server.Internal.Types

-- | The type of AUTH.
data UsernamePassword

instance FromHeaders user => IsAuth UsernamePassword user where
  type AuthArgs UsernamePassword = '[UsernamePasswordSettings]
  runAuth _ _ = usernamePasswordAuthCheck

-- | Convenient conversion to the web developer's user data type.
class FromHeaders user where
  fromHeaders :: ByteString -> ByteString -> user

-- | Any settings we might want to use.
data UsernamePasswordSettings =
  UsernamePasswordSettings
    { usernamePasswordSettingsUsername :: ByteString
    , usernamePasswordSettingsPassword :: ByteString
    }

-- | Consume the headers to produce an 'AuthResult'.
usernamePasswordAuthCheck ::
     FromHeaders user => UsernamePasswordSettings -> AuthCheck user
usernamePasswordAuthCheck usernamePasswordSettings = AuthCheck check
  where
    check request =
      case lookup "Username" (requestHeaders request) of
        Nothing -> pure Indefinite
        Just username
          | username ==
              usernamePasswordSettingsUsername usernamePasswordSettings ->
            case lookup "Password" (requestHeaders request) of
              Just password
                | password ==
                    usernamePasswordSettingsPassword usernamePasswordSettings ->
                  pure (Authenticated (fromHeaders username password))
                | otherwise -> pure BadPassword
              Nothing -> pure Indefinite
          | otherwise -> pure NoSuchUser
