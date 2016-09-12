{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Spotify.Api.Auth where

import           Control.Monad.Trans
import           Control.Monad.Trans.Except

import           Data.Aeson             (FromJSON(..), ToJSON(..), decode)
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

authBaseUrl = BaseUrl Https "accounts.spotify.com" 443 "/api/token"

newtype ClientId = ClientId T.Text
newtype ClientSecret = ClientSecret T.Text
data Credentials = Credentials ClientId ClientSecret

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
    maybe (throwE $ jsonDecodeErr clientAuthResp) return $ decode clientAuthResp 
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
   
    jsonDecodeErr :: BSL.ByteString -> ServantError
    jsonDecodeErr reqBody = DecodeFailure 
      "Could not parse ClientAuthResp" 
      ("application" // "json") 
      reqBody 


