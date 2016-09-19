{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Spotify.Api.Auth where

import           Control.Monad.Trans
import           Control.Monad.Trans.Except

import           Data.Aeson             hiding (encode)            
import           Data.Aeson.Types       (Options(..), defaultOptions)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.ByteString.Char8  as BSC
import           Data.ByteString.Base64 (encode)
import           Data.Coerce            (coerce)
import           Data.List              (intercalate, concat)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T 
import qualified Data.Text.Encoding     as T

import           GHC.Generics           (Generic)

import           Network.HTTP.Client 
import           Network.HTTP.Types     hiding (Header)
import           Network.HTTP.Media     

import           Servant.Client         hiding (responseBody)

clientAuthBaseUrl = BaseUrl Https "accounts.spotify.com" 443 "/api/token"
userAuthBaseUrl   = BaseUrl Https "accounts.spotify.com" 443 "/authorize"

newtype RedirectURI = RedirectURI T.Text
  deriving (Generic, Show)

newtype UserAuthData = UserAuthData (T.Text, Maybe T.Text)

data UserAuthResp = UserAuthResp
  { uaresp_access_token  :: T.Text
  , uaresp_token_type    :: T.Text
  , uaresp_scope         :: T.Text
  , uaresp_expires_in    :: Int
  , uaresp_refresh_token :: T.Text
  } deriving (Generic,Show)

instance ToJSON UserAuthResp where 
   toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = (++) "uaresp_" }

instance FromJSON UserAuthResp where
   parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 7 } 
 
data UserAuthReq = UserAuthReq 
  { uareq_code         :: T.Text
  , uareq_redirect_uri :: RedirectURI
  } deriving (Generic, Show)

userAuthClient :: UserAuthReq -> Credentials -> Manager -> BaseUrl -> ClientM UserAuthResp
userAuthClient (UserAuthReq code (RedirectURI redirUri)) creds manager baseUrl = do
    userAuthResp <- liftIO $ responseBody <$> httpLbs userAuthReq manager
    maybe (throwE $ jsonDecodeErr userAuthResp) return $ decode userAuthResp 
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

newtype ClientId = ClientId T.Text deriving (Show)
newtype ClientSecret = ClientSecret T.Text deriving (Show)
data Credentials = Credentials 
  { clientId :: ClientId 
  , clientSecret :: ClientSecret
  } deriving (Generic, Show)

instance FromJSON Credentials where
  parseJSON (Object v) = Credentials <$>
    (ClientId <$> v .: "clientId") <*>
    (ClientSecret <$> v .: "clientSecret") 

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


