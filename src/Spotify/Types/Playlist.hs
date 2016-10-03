{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Spotify.Types.Playlist where

import           Data.Aeson               
import           Data.Aeson.Types         (Options(..), defaultOptions)
import           Data.Text                (Text, unpack)
import           Data.Time
import           GHC.Generics             (Generic)

import qualified Spotify.Types.PagingObject as PO
import qualified Spotify.Types.Track        as T

-- | Spotify Playlist Object
data Playlist = Playlist
  { pl_collaborative   :: Bool 
  , pl_description     :: Maybe Text 
  , pl_external_urls   :: Object
  , pl_followers       :: Object
  , pl_href            :: Maybe Text
  , pl_id              :: Text
  , pl_images          :: [Object]
  , pl_name            :: Text
  , pl_owner           :: Object
  , pl_public          :: Maybe Bool
  , pl_snapshot_id     :: Text
  , pl_tracks          :: PO.PagingObject [PlaylistTrack]
  , pl_type            :: Text
  , pl_uri             :: Text
  } deriving (Generic, Show)

instance ToJSON Playlist where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = (++) "pl_" }

instance FromJSON Playlist where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 3 }

data PlaylistTrack = PlaylistTrack 
  { pltr_added_at :: Maybe UTCTime
  , pltr_added_by :: Object
  , pltr_is_local :: Bool
  , pltr_track    :: T.Track
  } deriving (Generic, Show)

instance ToJSON PlaylistTrack where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = (++) "pltr_" }

instance FromJSON PlaylistTrack where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 5 }

data CreatePlaylist = CreatePlaylist
  { name   :: Text
  , public :: Bool
  } deriving (Generic, Show)

instance ToJSON CreatePlaylist 
instance FromJSON CreatePlaylist 
