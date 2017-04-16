{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Spotify.Api where

import           Data.Aeson               (FromJSON(..), ToJSON(..), decode, Object)
import           Data.Proxy               (Proxy(..))
import qualified Data.Text                as T

import           GHC.Generics             (Generic)

import           Network.HTTP.Client      hiding (Proxy)   
import           Network.HTTP.Client.TLS  (tlsManagerSettings)

import           Servant.API
import           Servant.Client

import qualified Spotify.Auth.User -- for ToForm instance 

import qualified Spotify.Types.PagingObject as PO
import qualified Spotify.Types.Playlist     as PL
import qualified Spotify.Types.Track        as T
import qualified Spotify.Types.User         as U
import qualified Spotify.Types.Auth         as A

spotifyBaseUrl, spotifyAuthURL :: BaseUrl
spotifyBaseUrl = BaseUrl Https "api.spotify.com" 443 ""
spotifyAuthURL = BaseUrl Https "accounts.spotify.com" 443 ""

newtype SpotifyApiEnv = SpotifyApiEnv 
  { getSpotifyApiEnv :: ClientEnv } 

mkSpotifyApiEnv :: IO SpotifyApiEnv 
mkSpotifyApiEnv = do
  manager <- newManager tlsManagerSettings
  return $ SpotifyApiEnv $ ClientEnv manager spotifyBaseUrl

runSpotifyApiClientM :: ClientM a -> SpotifyApiEnv -> IO (Either ServantError a)
runSpotifyApiClientM clientM = runClientM clientM . getSpotifyApiEnv 

-- Note:
--   data ItemTypes = TrackItems | ArtistItems
--   w/ custom to/fromJSON
data TrackResponse = TrackResponse
  { tracks :: PO.PagingObject T.Track } deriving (Generic,Show)

instance FromJSON TrackResponse
instance ToJSON TrackResponse

type SpotifyAPI = "v1" :>
  (    Header "Authorization" T.Text :> SpotifySearchAPI
  :<|> Header "Authorization" A.UserAccessToken :> SpotifyUserAPI
  )

data SpotifyClient = SpotifyClient
  { mkSearchAPI :: Maybe T.Text -> SearchClient
  , mkUserAPI :: Maybe A.UserAccessToken -> UserClient
  }

spotifyAuthAPI :: Proxy SpotifyAuthAPI
spotifyAuthAPI = Proxy

type ClientAuthEndpoint =
       "api" :> "token"
       :> Header "Authorization" T.Text
       :> ReqBody '[FormUrlEncoded] A.ClientAuthReq
       :> Post '[JSON] A.ClientAuthResp

clientAuthEndpoint :: Proxy UserAuthEndpoint
clientAuthEndpoint = Proxy

type UserAuthEndpoint =
       "authorize"
       :> QueryParam "client_id" T.Text
       :> QueryParam "response_type" T.Text
       :> QueryParam "redirect_uri" T.Text
       :> QueryParam "state" T.Text
       :> QueryParam "scope" T.Text
       :> QueryParam "show_dialog" T.Text
       :> Get '[JSON] NoContent

userAuthEndpoint :: Proxy UserAuthEndpoint
userAuthEndpoint = Proxy

type SpotifyAuthAPI =
       ClientAuthEndpoint
  :<|> UserAuthEndpoint
  :<|> "api" :> "token"
       :> Header "Authorization" T.Text
       :> ReqBody '[FormUrlEncoded] A.UserAuthReq
       :> Post '[JSON] A.UserAuthResp

data SpotifyAuthClient = SpotifyAuthClient
  { authClient :: Maybe T.Text
               -> A.ClientAuthReq
               -> ClientM A.ClientAuthResp

  , authUser   :: Maybe T.Text
               -> Maybe T.Text
               -> Maybe T.Text
               -> Maybe T.Text
               -> Maybe T.Text
               -> Maybe T.Text
               -> ClientM NoContent

  , token      :: Maybe T.Text
               -> A.UserAuthReq
               -> ClientM A.UserAuthResp
  }

type SpotifySearchAPI = "search"
  :> QueryParam "q" T.Text
  :> QueryParam "type" T.Text
  :> QueryParam "market" T.Text
  :> QueryParam "limit" Int
  :> QueryParam "offset" Int
  :> Get '[JSON] TrackResponse

data SearchClient = SearchClient
  { searchTracks :: Maybe T.Text
                 -> Maybe T.Text
                 -> Maybe T.Text
                 -> Maybe Int
                 -> Maybe Int
                 -> ClientM TrackResponse
  } 

newtype UserID = UserID T.Text
type SpotifyUserAPI =
       "me" :> Get '[JSON] U.User
  :<|> "users" :>
       Capture "user_id" T.Text :>
       "playlists" :>
       ReqBody '[JSON] PL.CreatePlaylist :>
       Post '[JSON] PL.Playlist
  :<|> "users" :>
       Capture "user_id" T.Text :>
       "playlists" :>
       Capture "playlist_id" T.Text :>
       "tracks" :>
       QueryParam "uris" T.Text :> -- maybe TrackURI type
       Post '[JSON] Object -- change to json { "snapshot_id" : "zZyq..unLV" }

data UserClient = UserClient
  { me :: ClientM U.User
  , createPlaylist :: T.Text -> PL.CreatePlaylist -> ClientM PL.Playlist 
  , addTracksToPlaylist :: T.Text -> T.Text -> Maybe T.Text -> ClientM Object
  }

spotifyAPI :: Proxy SpotifyAPI
spotifyAPI = Proxy

mkSpotifyAuthClient :: SpotifyAuthClient
mkSpotifyAuthClient = SpotifyAuthClient{..}
  where
    (authClient :<|> authUser :<|> token) = client spotifyAuthAPI

mkSpotifyAPIClient :: SpotifyClient
mkSpotifyAPIClient = SpotifyClient{..}
  where
    (searchAPI :<|> userAPI) = client spotifyAPI

    mkSearchAPI :: Maybe T.Text -> SearchClient
    mkSearchAPI authHeader = SearchClient{..}
      where
        searchTracks = searchAPI authHeader

    mkUserAPI :: Maybe A.UserAccessToken -> UserClient
    mkUserAPI uaTok = UserClient{..}
      where
        (me :<|> createPlaylist :<|> addTracksToPlaylist) = userAPI uaTok

