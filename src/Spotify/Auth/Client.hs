{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Spotify.Auth.Client where

import           Control.Monad
import           Control.Monad.Reader     (ask)
import           Control.Monad.Except     (throwError)
import           Control.Monad.Trans

import           Data.Aeson               hiding (encode)
import           Data.Aeson.Types         (Options(..), defaultOptions)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.ByteString.Char8    as BSC
import           Data.HashMap.Strict      (fromList)
import           Data.Monoid              ((<>))
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T

import           Network.HTTP.Client 
import           Network.HTTP.Client.TLS  (tlsManagerSettings)  
import           Network.HTTP.Types       hiding (Header)
import           Network.HTTP.Media        

import           Servant.Client           hiding (responseBody)
import           Servant.Common.Req       hiding (responseBody)

import           Spotify.Types.Auth

import           Web.HttpApiData          (toQueryParam)
import           Web.FormUrlEncoded       (ToForm(..), Form(..))

-- Orphan because GHCJS :(
instance ToForm ClientAuthReq where
  toForm (ClientAuthReq gt) = [("grant_type", toQueryParam gt)]

clientAuthBaseUrl :: BaseUrl
clientAuthBaseUrl = BaseUrl Https "accounts.spotify.com" 443 ""

newtype SpotifyClientAuthEnv = SpotifyClientAuthEnv 
  { getSpotifyClientAuthEnv :: ClientEnv } 

mkSpotifyClientAuthEnv :: IO SpotifyClientAuthEnv 
mkSpotifyClientAuthEnv = do
  manager <- newManager tlsManagerSettings
  return $ SpotifyClientAuthEnv $ ClientEnv manager clientAuthBaseUrl 

runSpotifyClientAuthClientM :: ClientM a -> SpotifyClientAuthEnv -> IO (Either ServantError a)
runSpotifyClientAuthClientM clientM = runClientM clientM . getSpotifyClientAuthEnv 
