{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Soundcloud.Data.Track where

import           Data.Aeson 
import           Data.Aeson.Types         (Options(..), defaultOptions)
import           Data.Text                (Text)
import           Data.Time
import           GHC.Generics             (Generic)

data Track = Track 
  { artwork_url   :: Text
  , attachments_uri :: Text
  , bpm           :: Maybe Int
  , comment_count :: Int
  , commentable   :: Bool
  , created_at     :: Text -- Can't decode as UTCTime
  , description    :: Text
  , download_count :: Int
  , downloadable   :: Bool
  , download_url   :: Maybe Text
  , duration        :: Int
  , embeddable_by  :: Text
  , favoritings_count :: Int
  , id              :: Int
  , isrc            :: Maybe Text
  , key_signature   :: Text
  , kind            :: Text
  , label_id        :: Maybe Int
  , label_name      :: Maybe Text
  , last_modified   :: Text -- Can't decode as UTCTime :(
  , license         :: Text
  , original_content_size :: Int -- # bytes
  , original_format :: Text
  , permalink     :: Text
  , permalink_url :: Text 
  , playback_count :: Int
  , purchase_title :: Maybe Text
  , purchase_url  :: Maybe Text
  , release       :: Text 
  , release_day   :: Maybe Int 
  , release_month :: Maybe Int 
  , release_year  :: Maybe Int 
  , sharing       :: Text
  , state         :: Text
  , streamable    :: Bool
  , stream_url    :: Text
  , tag_list      :: Text
  , title         :: Text
  , track_type    :: Maybe Text
  , uri           :: Text
  , user          :: TrackUser
  , user_id       :: Int
  , user_playback_count :: Maybe Int
  , video_url     :: Maybe Text
  , waveform_url  :: Text
  } deriving (Generic, Show)

instance ToJSON Track
instance FromJSON Track 

data TrackUser = TrackUser
  { u_avatar_url :: Text
  , u_id :: Int
  , u_kind :: Text
  , u_permalink :: Text
  , u_permalink_url :: Text
  , u_uri :: Text
  , u_username :: Text
  } deriving (Generic, Show)

instance ToJSON TrackUser where
  toEncoding = genericToEncoding $ -- remove 'u_'
    defaultOptions { constructorTagModifier = drop 2 }

instance FromJSON TrackUser where
  parseJSON = genericParseJSON $ 
    defaultOptions { fieldLabelModifier = drop 2 }
