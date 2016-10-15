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

import qualified Spotify.Types.PagingObject as PO
import qualified Spotify.Types.Playlist     as PL
import qualified Spotify.Types.Track        as T 
import qualified Spotify.Types.User         as U 
import qualified Spotify.Auth.User      as UA

spotifyBaseUrl = BaseUrl Https "api.spotify.com" 443 ""

-- Note:
--   data ItemTypes = TrackItems | ArtistItems 
--   w/ custom to/fromJSON 
data TrackResponse = TrackResponse
  { tracks :: PO.PagingObject T.Track } deriving (Generic,Show)

instance FromJSON TrackResponse
instance ToJSON TrackResponse

type SpotifyAPI = "v1" :>  
  (    Header "Authorization" T.Text :> SpotifySearchAPI
  :<|> Header "Authorization" UA.UserAccessToken :> SpotifyUserAPI
  )

data SpotifyClient = SpotifyClient 
  { mkSearchAPI :: Maybe T.Text -> SearchClient 
  , mkUserAPI :: Maybe UA.UserAccessToken -> UserClient 
  } 

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

newtype UserID = UserID T.Text
type SpotifyUserAPI = 
      "me" :> Get '[JSON] U.User
  :<|> "users" :>
      ( Capture "user_id" T.Text :> 
        "playlists" :> 
        ReqBody '[JSON] PL.CreatePlaylist :> 
        Post '[JSON] PL.Playlist   
      )

data UserClient = UserClient
  { me :: Manager -> BaseUrl -> ClientM U.User
  , createPlaylist :: T.Text -> PL.CreatePlaylist -> 
                      Manager -> BaseUrl -> ClientM PL.Playlist 
  }

spotifyAPI :: Proxy SpotifyAPI 
spotifyAPI = Proxy

makeSpotifyAPIClient :: SpotifyClient
makeSpotifyAPIClient = SpotifyClient{..}
  where
    (searchAPI :<|> userAPI) = client spotifyAPI

    mkSearchAPI :: Maybe T.Text ->SearchClient
    mkSearchAPI authHeader = SearchClient{..}
      where
        searchTracks = searchAPI authHeader 
    
    mkUserAPI :: Maybe UA.UserAccessToken -> UserClient
    mkUserAPI uaTok = UserClient{..} 
      where 
        (me :<|> createPlaylist) = userAPI uaTok

