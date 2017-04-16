{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Spotify.Auth.User where

import           Control.Monad
import           Control.Monad.Reader    (ask)
import           Control.Monad.Trans     (liftIO)
import           Control.Monad.Except

import           Data.Aeson              hiding (encode)
import           Data.Aeson.Types        (Options(..), defaultOptions)
import           Data.Bifunctor          (second)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteString.Char8   as BSC
import           Data.ByteString.Base64  (encode)
import           Data.HashMap.Strict     (fromList)
import           Data.List               (intercalate, concat)
import           Data.Monoid             ((<>))
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T

import           GHC.Generics            (Generic)

import           Network.HTTP.Client
import           Network.HTTP.Types      hiding (Header)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Media      

import           Web.FormUrlEncoded      (ToForm(..), Form(..))

import           Servant.Client          hiding (responseBody)

import           Spotify.Auth.Client 

import           Spotify.Types.Auth   
import qualified Spotify.Types.User      as U 

-- TODO use generics to get field names
instance ToForm UserAuthReq where
  toForm (UserAuthReq gt c ru) = Form $ fromList $ 
    zip ["grant_type","code","redirect_uri"] [[gt],[c],[ru]]

userAuthBaseUrl :: BaseUrl
userAuthBaseUrl = BaseUrl Https "accounts.spotify.com" 443 "/authorize"

newtype SpotifyUserAuthEnv = SpotifyUserAuthEnv 
  { getSpotifyUserAuthEnv :: ClientEnv } 

mkSpotifyUserAuthEnv :: IO SpotifyUserAuthEnv 
mkSpotifyUserAuthEnv = do
  manager <- newManager tlsManagerSettings
  return $ SpotifyUserAuthEnv $ ClientEnv manager userAuthBaseUrl 

runSpotifyUserAuthClientM :: ClientM a -> SpotifyUserAuthEnv -> IO (Either ServantError a)
runSpotifyUserAuthClientM clientM = runClientM clientM . getSpotifyUserAuthEnv 
