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

-- NOTE:
--   make ADT for 
-- https://accounts.spotify.com/en/authorize?client_id=ddd07e8d90794e479f14a721c313a032&response_type=code&redirect_uri=http%3A%2F%2Flocalhost%3A3000&scope=playlist-modify-public

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
  } deriving (Show, Generic)

instance ToJSON UserAuthResp where 
   toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = drop 6 }
instance FromJSON UserAuthResp where
   parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 6 } 

data UserLoginReq = UserLoginReq
  { ulreq_client_id    :: ClientId 
  , ulreq_redirect_uri :: RedirectURI 
  , ulreq_state        :: Maybe T.Text
  , ulreq_scope        :: Maybe T.Text
  , ulreq_show_dialog  :: Maybe T.Text
  } deriving (Generic, Show) 

instance ToJSON UserLoginReq where 
   toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = drop 6 }
instance FromJSON UserLoginReq where
   parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 6 } 

mkUserLoginReq :: ClientId -> RedirectURI -> T.Text -> UserLoginReq
mkUserLoginReq cid redirUri scope = UserLoginReq cid redirUri Nothing (Just scope) Nothing

-- User & response from Spotify are sent to 'redirect_uri'
mkLoginReq :: UserLoginReq -> SpotifyUserAuthEnv -> Request
mkLoginReq userLoginReq env = userLoginRequest 
  where
    (UserLoginReq cId redirUri mState mScope mShowDialog) = userLoginReq
    (SpotifyUserAuthEnv (ClientEnv manager baseUrl)) = env

    userLoginRequest :: Request
    userLoginRequest = defaultRequest
      { host = BSC.pack $ baseUrlHost baseUrl
      , path = BSC.pack $ baseUrlPath baseUrl
      , port = baseUrlPort baseUrl
      , queryString = renderQuery True $ 
          map (second $ fmap T.encodeUtf8) $
            [ ("client_id", Just $ getClientId cId)
            , ("redirect_uri", Just $ getRedirectURI redirUri)
            , ("response_type", Just "code") -- always the same
            , ("state", mState)
            , ("scope", mScope)
            , ("show_dialog", mShowDialog)
            ]
      , secure = True
      }

data UserAuthReq = UserAuthReq 
  { uareq_code         :: T.Text
  , uareq_redirect_uri :: RedirectURI
  } deriving (Generic, Show)

userAuthClient :: UserAuthReq -> Credentials -> ClientM UserAuthResp
userAuthClient (UserAuthReq code (RedirectURI redirUri)) creds = do
    (ClientEnv manager baseUrl) <- ask 
    userAuthResp <- liftIO $ responseBody <$> httpLbs (mkUserAuthReq baseUrl) manager
    either (throwError . jsonDecodeErr) return $ eitherDecode userAuthResp 
  where
    
    mkUserAuthReq :: BaseUrl -> Request
    mkUserAuthReq baseUrl = defaultRequest 
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

