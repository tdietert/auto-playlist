{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Spotify.Types.User where

import           Data.Aeson               
import           Data.Aeson.Types         (Options(..), defaultOptions)
import           Data.Text                (Text, unpack)
import           Data.Time
import           GHC.Generics             (Generic)

import qualified Spotify.Types.PagingObject as PO
import qualified Spotify.Types.Track        as T

data UserPublic = UserPublic
  { upub_display_name  :: Text
  , upub_external_urls :: Object
  , upub_followers     :: Object
  , upub_href          :: Text
  , upub_id            :: Text
  , upub_images        :: [Object]
  , upub_type          :: Text
  , upub_uri           :: Text
  } deriving (Generic,Show)

instance FromJSON UserPublic where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = (++) "upub_" }

instance ToJSON UserPublic where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = drop 5 }

data UserPrivate = UserPrivate
  { upriv_birthdate      :: Text
  , upriv_country        :: Text
  , upriv_display_name  :: Text
  , upriv_email          :: Text
  , upriv_external_urls :: Object
  , upriv_followers     :: Object
  , upriv_href          :: Text
  , upriv_id            :: Text
  , upriv_images        :: [Object]
  , upriv_product        :: Text
  , upriv_type          :: Text
  , upriv_uri           :: Text
  } deriving (Generic,Show)

instance FromJSON UserPrivate where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = (++) "upriv_" }

instance ToJSON UserPrivate where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = drop 6 }

