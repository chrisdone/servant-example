{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib where

import Data.Aeson
import Data.Time (UTCTime)
import GHC.Generics

import Servant.API -- from servant package

{-

* Verbs method type return

* return can be NoContent

* request body
* content types '[JSON]
* alternating :<|>
* query params
* captures
* plain text parts
* request headers
* reply headers
* basic auth
* empty API
* raw

-}

-- data Verb method (statusCode :: Nat) (contentType :: [*]) a
-- type Get = Verb 'GET 200
-- NoContent for empty

type UserAPI2 = "users" :> "list-all" :> ReqBody '[JSON] User :> Post '[JSON] [User]
           -- :<|> "list-all" :> QueryParam "sort" SortBy :> Capture "limit" Integer :> "users" :> Get '[JSON] [User]
           :<|> "list-all" :> Header "User-Agent" String :> "users"
                 :> Get '[JSON] (Headers '[Header "User-Count" Integer] [User])
           -- :<|> BasicAuth "my-realm" User :> EmptyAPI
           -- :<|> Raw
           :<|> EmptyAPI

data SortBy = Age | Name
 deriving (Generic)
instance FromJSON SortBy
instance ToJSON SortBy

data User = User {
  name :: String,
  age :: Int,
  email :: String,
  registration_date :: UTCTime
 } deriving (Generic)

instance FromJSON User
instance ToJSON User
