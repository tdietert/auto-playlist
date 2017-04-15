{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spotify.Auth.Client where

import           Control.Monad
import           Control.Monad.Reader     (ask)
import           Control.Monad.Except     (throwError)
import           Control.Monad.Trans

import           Data.Aeson               hiding (encode)
import           Data.Aeson.Types         (Options(..), defaultOptions)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.ByteString.Char8    as BSC
import           Data.ByteString.Base64   (encode)
import           Data.Coerce              (coerce)
import           Data.HashMap.Strict      (fromList)
import           Data.Monoid              ((<>))
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T

import           GHC.Generics             (Generic)

import           Network.HTTP.Client 
import           Network.HTTP.Client.TLS  (tlsManagerSettings)  
import           Network.HTTP.Types       hiding (Header)
import           Network.HTTP.Media        

import           Servant.Client           hiding (responseBody)
import           Servant.Common.Req       hiding (responseBody)
import           Web.Internal.FormUrlEncoded (ToForm(..), Form(..))

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

data ClientAuthReq = ClientAuthReq
  { grant_type :: T.Text 
  } deriving (Show, Generic, ToJSON, FromJSON)

instance ToForm ClientAuthReq where
  toForm (ClientAuthReq gt) = Form $ fromList [("grant_type", [gt])]

basicClientAuthReq :: ClientAuthReq
basicClientAuthReq = ClientAuthReq "client_credentials"

data ClientAuthResp = ClientAuthResp
  { access_token :: T.Text
  , token_type :: T.Text
  , expires_in :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)

mkAuthHeaderFromCredsClient :: Credentials -> T.Text
mkAuthHeaderFromCredsClient (Credentials clientId clientSecret) = (<>) "Basic " $
  T.decodeUtf8 . encode . T.encodeUtf8 $ coerce clientId <> ":" <> coerce clientSecret

