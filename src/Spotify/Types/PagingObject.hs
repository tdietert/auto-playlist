{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Spotify.Types.PagingObject where

import           Data.Aeson               
import           Data.Aeson.Types         (Options(..), defaultOptions)
import           Data.List                (foldl', intercalate)
import           Data.Text                (Text, unpack)
import           Data.Time
import           GHC.Generics             (Generic)

import qualified Spotify.Types.Track      as T

-- | Spotify PagingObject 
data PagingObject a = PagingObject
  { po_href :: Text
  , po_items :: [a]
  , po_limit :: Int
  , po_next     :: Maybe Text
  , po_offset   :: Int
  , po_previous :: Maybe Text
  , po_total    :: Int
  } deriving (Generic, Show)

instance ToJSON a => ToJSON (PagingObject a) where
  toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = (++) "po_" } 

instance FromJSON a => FromJSON (PagingObject a) where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = drop 3 } 
