{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Spotify.Data.Track where

import           Data.Aeson               
import           Data.Aeson.Types         (Options(..), defaultOptions)
import           Data.Text                (Text)
import           Data.Time
import           GHC.Generics             (Generic)

data PagingObject = PagingObject
  { po_href :: Text
  , po_items :: [Track]
  , po_limit :: Int
  , po_next     :: Text
  , po_offset   :: Int
  , po_previous :: Maybe Text
  , po_total    :: Int
  } deriving (Generic, Show)

instance ToJSON PagingObject where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = (++) "po_" }

instance FromJSON PagingObject where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 3 } 

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
  } deriving (Generic, Show)

instance ToJSON Track where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = (++) "track_" }

instance FromJSON Track where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 6 }
