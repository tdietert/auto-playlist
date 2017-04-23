{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Spotify.Types.Auth where

import           Data.Aeson               hiding (encode)
import           Data.Aeson.Types         (Options(..), defaultOptions)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Base64   as BS64
import           Data.Monoid
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T

import           GHC.Generics             (Generic)

import qualified Spotify.Types.User       as U

import           Web.HttpApiData
 
-----------------------
-- Server Client Auth
-----------------------

newtype ClientId = ClientId T.Text
  deriving (Show, Generic, ToJSON, FromJSON)

newtype ClientSecret = ClientSecret T.Text
  deriving (Show, Generic, ToJSON, FromJSON)

data Credentials = Credentials
  { clientId :: ClientId
  , clientSecret :: ClientSecret
  } deriving (Show, Generic, ToJSON, FromJSON)

data ClientAuthReq = ClientAuthReq
  { grant_type :: T.Text 
  } deriving (Show, Generic, ToJSON, FromJSON)

basicClientAuthReq :: ClientAuthReq
basicClientAuthReq = ClientAuthReq "client_credentials"

mkAuthHeaderFromCredsClient :: Credentials -> T.Text
mkAuthHeaderFromCredsClient (Credentials (ClientId cid) (ClientSecret csec) ) = (<>) "Basic " $
  T.decodeUtf8 . BS64.encode . T.encodeUtf8 $ cid <> ":" <> csec

data ClientAuthResp = ClientAuthResp
  { access_token  :: T.Text
  , token_type    :: T.Text
  , expires_in    :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)

-----------------------
-- User Auth
-----------------------

data LoggedIn = LoggedIn (Maybe U.User) | NotLoggedIn
  deriving (Generic, ToJSON, FromJSON)

newtype RedirectURI = RedirectURI T.Text
  deriving (Generic, Show, ToJSON, FromJSON)

newtype Code = Code T.Text
  deriving (Generic, Show, ToJSON, FromJSON)

newtype UserAccessToken = UserAccessToken T.Text
  deriving (Show, Generic)

instance ToJSON UserAccessToken where
  toJSON (UserAccessToken uatok) = toJSON uatok
instance FromJSON UserAccessToken where
  parseJSON = fmap UserAccessToken . parseJSON

instance ToHttpApiData UserAccessToken where
  toUrlPiece (UserAccessToken uatok) = uatok 
  toQueryParam (UserAccessToken uatok) = uatok 
  toHeader (UserAccessToken uatok) = "Bearer " <> T.encodeUtf8 uatok

data UserAuthResp = UserAuthResp
  { uaresp_access_token  :: UserAccessToken
  , uaresp_token_type    :: T.Text
  , uaresp_expires_in    :: Int
  , uaresp_refresh_token :: T.Text
  , uaresp_scope         :: T.Text
  } deriving (Generic, Show)

instance ToJSON UserAuthResp where 
   toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = drop 7 }
instance FromJSON UserAuthResp where
   parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 7 } 

data UserAuthReq = UserAuthReq
  { uareq_grant_type   :: T.Text
  , uareq_code         :: T.Text
  , uareq_redirect_uri :: T.Text
  } deriving (Generic, Show)

instance ToJSON UserAuthReq where 
   toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = drop 6 }
instance FromJSON UserAuthReq where
   parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 6 } 

mkUserAuthReq :: T.Text -> Code -> RedirectURI -> UserAuthReq
mkUserAuthReq grantType (Code code) (RedirectURI redirUri) =
  UserAuthReq grantType code redirUri
