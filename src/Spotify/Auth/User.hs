{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spotify.Auth.User where

import           Control.Monad
import           Control.Monad.Reader    (ask)
import           Control.Monad.Trans     (liftIO)
import           Control.Monad.Except

import           Data.Aeson              hiding (encode)
import           Data.Aeson.Types        (Options(..), defaultOptions)
import           Data.Bifunctor          (second)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteString.Char8   as BSC
import           Data.ByteString.Base64  (encode)
import           Data.HashMap.Strict     (fromList)
import           Data.List               (intercalate, concat)
import           Data.Monoid             ((<>))
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T

import           GHC.Generics            (Generic)

import           Network.HTTP.Client
import           Network.HTTP.Types      hiding (Header)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Media      

import           Web.HttpApiData
import           Web.Internal.FormUrlEncoded (ToForm(..), Form(..))

import           Servant.Client          hiding (responseBody)
import           Spotify.Auth.Client 
import qualified Spotify.Types.User         as U 

userAuthBaseUrl :: BaseUrl
userAuthBaseUrl = BaseUrl Https "accounts.spotify.com" 443 "/authorize"

newtype SpotifyUserAuthEnv = SpotifyUserAuthEnv 
  { getSpotifyUserAuthEnv :: ClientEnv } 

mkSpotifyUserAuthEnv :: IO SpotifyUserAuthEnv 
mkSpotifyUserAuthEnv = do
  manager <- newManager tlsManagerSettings
  return $ SpotifyUserAuthEnv $ ClientEnv manager userAuthBaseUrl 

runSpotifyUserAuthClientM :: ClientM a -> SpotifyUserAuthEnv -> IO (Either ServantError a)
runSpotifyUserAuthClientM clientM = runClientM clientM . getSpotifyUserAuthEnv 

data LoggedIn = LoggedIn (Maybe U.User) | NotLoggedIn
  deriving (Generic, ToJSON, FromJSON)

newtype RedirectURI = RedirectURI 
  { getRedirectURI :: T.Text }
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

-- | TODO use generics to get field names
instance ToForm UserAuthReq where
  toForm (UserAuthReq gt c ru) = Form $ fromList $ 
    zip ["grant_type","code","redirect_uri"] [[gt],[c],[ru]]

mkUserAuthReq :: T.Text -> Code -> RedirectURI -> UserAuthReq
mkUserAuthReq grantType (Code code) (RedirectURI redirUri) =
  UserAuthReq grantType code redirUri
