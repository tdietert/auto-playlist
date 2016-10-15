{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Spotify.Types.Track where

import           Data.Aeson               
import           Data.Aeson.Types         (Options(..), defaultOptions)
import           Data.List                (foldl', intercalate)
import           Data.Text                (Text, unpack)
import           Data.Time
import           GHC.Generics             (Generic)

-- | Spotify Track Object
data Track = Track
  { track_album :: Object 
  , track_artists :: [Object]
  , track_available_markets :: [Text]
  , track_disc_number :: Int
  , track_duration_ms :: Int
  , track_explicit :: Bool
  , track_external_ids :: Object 
  , track_external_urls :: Object 
  , track_href          :: Text
  , track_id            :: Text
  , track_is_playable   :: Maybe Text
  , track_linked_from   :: Maybe Object 
  , track_name          :: Text
  , track_popularity    :: Int
  , track_preview_url   :: Maybe Text
  , track_track_number  :: Int
  , track_type          :: Maybe Text
  , track_uri           :: Maybe Text
  } deriving (Generic)

instance Show Track where
  show (Track{..}) = unwords 
    [ "SpotifyTrack" 
    , unpack track_id
    , unpack track_name
    , unpack track_href
    ]

instance ToJSON Track where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = drop 6 }

instance FromJSON Track where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 6 }
