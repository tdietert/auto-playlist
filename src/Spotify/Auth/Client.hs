{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spotify.Auth.Client where

import           Control.Monad
import           Control.Monad.Reader    (ask)
import           Control.Monad.Except    (throwError)
import           Control.Monad.Trans

import           Data.Aeson              hiding (encode)
import           Data.Aeson.Types        (Options(..), defaultOptions)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteString.Char8   as BSC
import           Data.ByteString.Base64  (encode)
import           Data.Coerce             (coerce)
import           Data.Monoid             ((<>))
import qualified Data.Text               as T 
import qualified Data.Text.Encoding      as T

import           GHC.Generics            (Generic)

import           Network.HTTP.Client 
import           Network.HTTP.Client.TLS (tlsManagerSettings)  
import           Network.HTTP.Types      hiding (Header)
import           Network.HTTP.Media       

import           Servant.Client          hiding (responseBody)
import           Servant.Common.Req      hiding (responseBody)

clientAuthBaseUrl :: BaseUrl
clientAuthBaseUrl = BaseUrl Https "accounts.spotify.com" 443 "/api/token"

newtype SpotifyClientAuthEnv = SpotifyClientAuthEnv 
  { getSpotifyClientAuthEnv :: ClientEnv } 

mkSpotifyClientAuthEnv :: IO SpotifyClientAuthEnv 
mkSpotifyClientAuthEnv = do
  manager <- newManager tlsManagerSettings
  return $ SpotifyClientAuthEnv $ ClientEnv manager clientAuthBaseUrl 

runSpotifyClientAuthClientM :: ClientM a -> SpotifyClientAuthEnv -> IO (Either ServantError a)
runSpotifyClientAuthClientM clientM = runClientM clientM . getSpotifyClientAuthEnv 

newtype ClientId = ClientId 
  { getClientId :: T.Text }
  deriving (Show, Generic, ToJSON, FromJSON)

newtype ClientSecret = ClientSecret 
  { getClientSecret :: T.Text } 
  deriving (Show, Generic, ToJSON, FromJSON)

data Credentials = Credentials 
  { clientId :: ClientId 
  , clientSecret :: ClientSecret
  } deriving (Show, Generic, ToJSON, FromJSON)

data ClientAuthResp = ClientAuthResp
  { access_token :: T.Text
  , token_type :: T.Text
  , expires_in :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)

class ToHeaderVal a where
  toHeaderVal :: a -> T.Text

instance ToHeaderVal ClientAuthResp where
  toHeaderVal (ClientAuthResp atok toktyp _) = toktyp <> " " <> atok

mkAuthHeaderFromCredsClient :: Credentials -> T.Text
mkAuthHeaderFromCredsClient (Credentials clientId clientSecret) = (<>) "Basic " $
  T.decodeUtf8 . encode . T.encodeUtf8 $ coerce clientId <> ":" <> coerce clientSecret

mkAuthHeaderFromCredsClientBS :: Credentials -> BS.ByteString
mkAuthHeaderFromCredsClientBS (Credentials clientId clientSecret) = (<>) "Basic " $
  encode . T.encodeUtf8 $ coerce clientId <> ":" <> coerce clientSecret

clientAuthClient :: Credentials -> ClientM ClientAuthResp
clientAuthClient creds = do
    (ClientEnv manager baseUrl) <- ask 
    clientAuthResp <- liftIO $ responseBody <$> 
      httpLbs (mkClientAuthReq baseUrl) manager
    case eitherDecode clientAuthResp of
      Left err -> throwError $ jsonDecodeErr err 
      Right resp -> return resp
  where 
    mkClientAuthReq :: BaseUrl -> Request
    mkClientAuthReq baseUrl = defaultRequest 
      { host = BSC.pack $ baseUrlHost baseUrl 
      , path = BSC.pack $ baseUrlPath baseUrl 
      , port = baseUrlPort baseUrl
      , requestHeaders = [(hAuthorization, mkAuthHeaderFromCredsClientBS creds),(hContentType, "application/x-www-form-urlencoded")]
      , requestBody = "grant_type=client_credentials" 
      , method = "POST"
      , secure = True
      }

-- migrate to Utils.hs
jsonDecodeErr :: String -> ServantError
jsonDecodeErr = DecodeFailure "Failed to parse: " ("application" // "json") . BSL.fromStrict . BSC.pack 

