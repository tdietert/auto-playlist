{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spotify.Auth.User where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Except

import           Data.Aeson              hiding (encode)
import           Data.Aeson.Types        (Options(..), defaultOptions)
import           Data.Bifunctor          (second)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteString.Char8   as BSC
import           Data.ByteString.Base64  (encode)
import           Data.Coerce             (coerce)
import           Data.List               (intercalate, concat)
import           Data.Monoid             ((<>))
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T

import           GHC.Generics            (Generic)

import           Network.HTTP.Client
import           Network.HTTP.Types      hiding (Header)
import           Network.HTTP.Media

import           Web.HttpApiData

import           Servant.Client          hiding (responseBody)
import           Spotify.Auth.Client
import qualified Spotify.Types.User         as U

userAuthBaseUrl   = BaseUrl Https "accounts.spotify.com" 443 "/authorize"

data LoggedIn = LoggedIn (Maybe U.User) | NotLoggedIn
  deriving (Generic, ToJSON, FromJSON)

newtype RedirectURI = RedirectURI T.Text
  deriving (Generic, Show, ToJSON, FromJSON)

newtype UserAccessToken = UserAccessToken T.Text
  deriving (Show, Generic)

instance ToJSON UserAccessToken where
  toJSON (UserAccessToken uatok) = toJSON uatok
instance FromJSON UserAccessToken where
  parseJSON = fmap UserAccessToken . parseJSON

instance ToHttpApiData UserAccessToken where
  toUrlPiece = coerce
  toQueryParam = coerce
  toHeader (UserAccessToken uatok) = "Bearer " <> T.encodeUtf8 uatok

-- no prefixes on fields because
-- querying Spotify API spec in Spotify/Api.hs
-- is local and decodes JSON from Spotify server,
-- so 2 prefixes are added: uaresp_uaresp_access_token.
data UserAuthResp = UserAuthResp
  { access_token  :: UserAccessToken
  , token_type    :: T.Text
  , expires_in    :: Int
  , refresh_token :: T.Text
  , scope         :: T.Text
  } deriving (Generic,Show)

instance ToJSON UserAuthResp
instance FromJSON UserAuthResp

data UserAuthReq = UserAuthReq
  { uareq_code         :: T.Text
  , uareq_redirect_uri :: RedirectURI
  } deriving (Generic, Show)

userAuthClient :: UserAuthReq -> Credentials -> Manager -> BaseUrl -> ClientM UserAuthResp
userAuthClient (UserAuthReq code (RedirectURI redirUri)) creds manager baseUrl = do
    userAuthResp <- liftIO $ responseBody <$> httpLbs userAuthReq manager
    either (throwE . jsonDecodeErr) return $ eitherDecode userAuthResp
  where
    userAuthReq :: Request
    userAuthReq = defaultRequest
      { host = BSC.pack $ baseUrlHost baseUrl
      , path = BSC.pack $ baseUrlPath baseUrl
      , port = baseUrlPort baseUrl
      , requestHeaders = [(hAuthorization, mkAuthHeaderFromCredsClientBS creds),(hContentType, "application/x-www-form-urlencoded")]
      , requestBody = RequestBodyBS $ BS.intercalate "&" $
          ["grant_type=authorization_code"
          ,"code=" <> T.encodeUtf8 code
          ,"redirect_uri=" <> T.encodeUtf8 redirUri
          ]
      , method = "POST"
      , secure = True
      }

