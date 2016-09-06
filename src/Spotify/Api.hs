{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Spotify.Api where

import           Data.Aeson             (FromJSON(..), ToJSON(..))
import           Data.Coerce            (coerce)
import           Data.Monoid            ((<>))
import           Data.Proxy             (Proxy(..))
import qualified Data.Text              as T

import           GHC.Generics           (Generic)

import           Network.HTTP.Client    (Manager, newManager, defaultManagerSettings)

import           Servant.API
import           Servant.Client

import qualified Spotify.Data.Track  as ST 

spotifyBaseUrl = BaseUrl Https "api.spotify.com/v1/" 80 ""

newtype ClientId = ClientId T.Text
newtype ClientSecret = ClientSecret T.Text
data Credentials = Credentials ClientId ClientSecret

mkAuthHeaderFromCreds :: Credentials -> T.Text
mkAuthHeaderFromCreds (Credentials clientId clientSecret) =
  "Basic " <> coerce clientId <> ":" <> coerce clientSecret

-- TODO
-- Add authorization endpoint that sends Auth header with Basic Auth
-- and add datatype for auth response. get access token, send with
--  Authentication | Bearer:<access_token>

data TrackResponse = TrackResponse
  { tracks :: ST.PagingObject } deriving (Generic,Show)

instance FromJSON TrackResponse
instance ToJSON TrackResponse

type SpotifyAPI = Header "Authorization" T.Text :> SpotifySearchAPI

data SpotifyClient = SpotifyClient 
  { mkSearchAPI :: Maybe T.Text -> SearchClient } 

type SpotifySearchAPI = "search" :> 
  QueryParam "q" T.Text :> 
  QueryParam "market" T.Text :>
  QueryParam "limit" Int :> 
  QueryParam "offset" Int :> Get '[JSON] TrackResponse 

data SearchClient = SearchClient
  { searchTracks :: Maybe T.Text -> Maybe T.Text -> Maybe Int -> Maybe Int -> Manager -> BaseUrl -> ClientM TrackResponse
  } 

spotifyAPI :: Proxy SpotifySearchAPI
spotifyAPI = Proxy

makeSpotifyAPIClient :: SpotifyClient
makeSpotifyAPIClient = SpotifyClient{..}
  where
    searchAPI = client (Proxy :: Proxy SpotifyAPI)

    mkSearchAPI :: Maybe T.Text ->SearchClient
    mkSearchAPI authHeader = SearchClient{..}
      where
        searchTracks = searchAPI authHeader 

