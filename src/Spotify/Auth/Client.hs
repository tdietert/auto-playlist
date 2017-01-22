{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spotify.Auth.Client where

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

import           Servant.API.ContentTypes (ToFormUrlEncoded(..))
import           Servant.Client           hiding (responseBody)

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

data ClientAuthReq = ClientAuthReq
  { grant_type :: T.Text
  }

instance ToFormUrlEncoded ClientAuthReq where
  toFormUrlEncoded (ClientAuthReq gt) = [("grant_type", gt)]

basicClientAuthReq = ClientAuthReq "client_credentials"

data ClientAuthResp = ClientAuthResp
  { access_token :: T.Text
  , token_type :: T.Text
  , expires_in :: Int
  } deriving (Generic, Show, FromJSON)

mkAuthHeaderFromCredsClient :: Credentials -> T.Text
mkAuthHeaderFromCredsClient (Credentials clientId clientSecret) = (<>) "Basic " $
  T.decodeUtf8 . encode . T.encodeUtf8 $ coerce clientId <> ":" <> coerce clientSecret

