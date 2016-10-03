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


data User = User
  { u_birthdate     :: Maybe Text
  , u_country       :: Maybe Text
  , u_display_name  :: Text
  , u_email         :: Maybe Text
  , u_external_urls :: Object
  , u_followers     :: Object
  , u_href          :: Text
  , u_id            :: Text
  , u_images        :: [Object]
  , u_product       :: Maybe Text
  , u_type          :: Text
  , u_uri           :: Text
  } deriving (Generic,Show)

instance FromJSON User where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 2 }
instance ToJSON User where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = (++) "u_" }


