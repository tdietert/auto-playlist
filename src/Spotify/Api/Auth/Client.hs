{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spotify.Api.Auth.Client where

import           Control.Monad
import           Control.Monad.Trans.Except (throwE)
import           Control.Monad.Trans

import           Data.Aeson             hiding (encode)
import           Data.Aeson.Types       (Options(..), defaultOptions)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.ByteString.Char8  as BSC
import           Data.ByteString.Base64 (encode)
import           Data.Coerce            (coerce)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T 
import qualified Data.Text.Encoding     as T

import           GHC.Generics           (Generic)

import           Network.HTTP.Client 
import           Network.HTTP.Types     hiding (Header)
import           Network.HTTP.Media     

import           Servant.Client         hiding (responseBody)

clientAuthBaseUrl = BaseUrl Https "accounts.spotify.com" 443 "/api/token"

newtype ClientId = ClientId T.Text deriving (Show, Generic)
instance ToJSON ClientId
instance FromJSON ClientId

newtype ClientSecret = ClientSecret T.Text deriving (Show, Generic)
instance ToJSON ClientSecret  
instance FromJSON ClientSecret 

data Credentials = Credentials 
  { clientId :: ClientId 
  , clientSecret :: ClientSecret
  } deriving (Show, Generic)

instance ToJSON Credentials  
instance FromJSON Credentials 

data ClientAuthResp = ClientAuthResp
  { access_token :: T.Text
  , token_type :: T.Text
  , expires_in :: Int
  } deriving (Generic, Show)

instance FromJSON ClientAuthResp 
instance ToJSON ClientAuthResp 

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

clientAuthClient :: Credentials -> Manager -> BaseUrl -> ClientM ClientAuthResp
clientAuthClient creds manager baseUrl = do
    clientAuthResp <- liftIO $ responseBody <$> httpLbs clientAuthReq manager
    either (throwE . jsonDecodeErr) return $ eitherDecode clientAuthResp 
  where 
    clientAuthReq :: Request
    clientAuthReq = defaultRequest 
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

