{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Soundcloud.Api where

import           Data.Proxy             (Proxy(..))
import qualified Data.Text              as T

import           Network.HTTP.Client    (Manager, newManager, defaultManagerSettings)

import           Servant.API
import           Servant.Client

import qualified Soundcloud.Data.Track  as ST 

scApiBaseURL = BaseUrl Http "api.soundcloud.com" 80 ""

type SoundcloudAPI = QueryParam "client_id" T.Text :> SoundcloudTrackAPI 

type SoundcloudTrackAPI = "tracks" :>    
  (    QueryParams "genres" T.Text :> Get '[JSON] [ST.Track] 
  :<|> Capture "id" Int :> Get '[JSON] ST.Track
  :<|> Capture "id" Int :> ReqBody '[JSON] ST.Track :> Put '[JSON] ()
  :<|> Capture "id" Int :> ReqBody '[JSON] ST.Track :> Delete '[JSON] () 
  )

data SoundcloudClient = SoundcloudClient 
  { mkTrackAPI :: Maybe T.Text -> TrackClient
  }

data TrackClient = TrackClient 
  { searchTracksByGenre :: [T.Text] -> Manager -> BaseUrl -> ClientM [ST.Track]
  , getTrack            :: Int -> Manager -> BaseUrl -> ClientM ST.Track
  , putTrack            :: Int -> ST.Track -> Manager -> BaseUrl -> ClientM ()
  , deleteTrack         :: Int -> ST.Track -> Manager -> BaseUrl -> ClientM ()
  } 
 
soundcloudAPI :: Proxy SoundcloudTrackAPI
soundcloudAPI = Proxy

makeAPIClient :: SoundcloudClient 
makeAPIClient = SoundcloudClient{..} 
  where
    trackAPI = client (Proxy :: Proxy SoundcloudAPI)

    mkTrackAPI :: Maybe T.Text -> TrackClient
    mkTrackAPI clientId = TrackClient{..}
      where 
        searchTracksByGenre :<|> getTrack :<|> putTrack :<|> deleteTrack = trackAPI clientId
