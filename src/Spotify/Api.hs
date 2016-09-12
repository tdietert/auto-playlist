{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Spotify.Api where

import           Data.Aeson             (FromJSON(..), ToJSON(..), decode)
import           Data.Proxy             (Proxy(..))
import qualified Data.Text              as T

import           GHC.Generics           (Generic)

import           Network.HTTP.Client    hiding (Proxy)   

import           Servant.API
import           Servant.Client         

import qualified Spotify.Data.Track  as ST 

spotifyBaseUrl = BaseUrl Https "api.spotify.com" 443 ""

data TrackResponse = TrackResponse
  { tracks :: ST.PagingObject } deriving (Generic,Show)

instance FromJSON TrackResponse
instance ToJSON TrackResponse

type SpotifyAPI = "v1" :>  Header "Authorization" T.Text :> SpotifySearchAPI

data SpotifyClient = SpotifyClient 
  { mkSearchAPI :: Maybe T.Text -> SearchClient } 

type SpotifySearchAPI = "search" :> 
  QueryParam "q" T.Text :> 
  QueryParam "type" T.Text :>
  QueryParam "market" T.Text :>
  QueryParam "limit" Int :> 
  QueryParam "offset" Int :> Get '[JSON] TrackResponse 

data SearchClient = SearchClient
  { searchTracks :: Maybe T.Text ->
                    Maybe T.Text ->
                    Maybe T.Text ->
                    Maybe Int -> 
                    Maybe Int -> 
                    Manager -> BaseUrl -> ClientM TrackResponse
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

