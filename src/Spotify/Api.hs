{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Spotify.Api where

import           Data.Aeson             (FromJSON(..), ToJSON(..))
import           Data.ByteString.Base64 (encode)
import           Data.Coerce            (coerce)
import           Data.Monoid            ((<>))
import           Data.Proxy             (Proxy(..))
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T


import           GHC.Generics           (Generic)

import           Network.HTTP.Client    (Manager, newManager, defaultManagerSettings)

import           Servant.API
import           Servant.Client

import qualified Spotify.Data.Track  as ST 

spotifyBaseUrl = BaseUrl Https "api.spotify.com/v1" 80 ""
authBaseUrl = BaseUrl Https "accounts.spotify.com/api" 80 ""

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

data TrackResponse = TrackResponse
  { tracks :: ST.PagingObject } deriving (Generic,Show)

instance FromJSON TrackResponse
instance ToJSON TrackResponse

type SpotifyAPI = Header "Authorization" T.Text :> SpotifySearchAPI

type SpotifyAuthAPI = "token" :>
  Header "Authorization" T.Text :>
  ReqBody '[FormUrlEncoded] [(T.Text,T.Text)] :> Post '[JSON] ClientAuthResp

type SpotifySearchAPI = "search" :> 
  QueryParam "q" T.Text :> 
  QueryParam "market" T.Text :>
  QueryParam "limit" Int :> 
  QueryParam "offset" Int :> Get '[JSON] TrackResponse 

data SpotifyAuthClient = SpotifyAuthClient
  { authorizeClient :: Maybe T.Text -> [(T.Text,T.Text)] -> Manager -> BaseUrl -> ClientM ClientAuthResp }

data SpotifyClient = SpotifyClient 
  { mkSearchAPI :: Maybe T.Text -> SearchClient } 

data SearchClient = SearchClient
  { searchTracks :: Maybe T.Text -> Maybe T.Text -> Maybe Int -> Maybe Int -> Manager -> BaseUrl -> ClientM TrackResponse
  } 

spotifyAuthAPI :: Proxy SpotifyAuthAPI
spotifyAuthAPI = Proxy

spotifyAPI :: Proxy SpotifySearchAPI
spotifyAPI = Proxy

makeSpotifyAuthAPIClient :: SpotifyAuthClient
makeSpotifyAuthAPIClient = SpotifyAuthClient $ client (Proxy :: Proxy SpotifyAuthAPI)

makeSpotifyAPIClient :: SpotifyClient
makeSpotifyAPIClient = SpotifyClient{..}
  where
    searchAPI = client (Proxy :: Proxy SpotifyAPI)

    mkSearchAPI :: Maybe T.Text ->SearchClient
    mkSearchAPI authHeader = SearchClient{..}
      where
        searchTracks = searchAPI authHeader 

