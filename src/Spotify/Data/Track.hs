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
  , po_items :: [Text]
  , po_limit :: Int
  , po_next     :: Text
  , po_offset   :: Int
  , po_previous :: Text
  , po_total    :: Int
  } deriving (Generic, Show)

instance ToJSON PagingObject where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = drop 3 }

instance FromJSON PagingObject where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = (++) "po_" } 

data Track = Track
  { track_album :: Text
  , track_artists :: [Text]
  , track_available_markets :: [Text]
  , track_disc_number :: Int
  , track_duration_ms :: Int
  , track_explicit :: Bool
  , track_external_ids :: Text
  , track_external_urls :: Text
  , track_href          :: Text
  , track_id            :: Text
  , track_is_playable   :: Text
  , track_linked_from   :: Text
  , track_name          :: Text
  , track_popularity    :: Int
  , track_preview_url   :: Text
  , track_track_number  :: Int
  , track_type          :: Text
  , track_uri           :: Text
  } deriving (Generic, Show)

instance ToJSON Track where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = drop 6 }

instance FromJSON Track where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = (++) "track_" }
