{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Except        (runExceptT)

import qualified Data.Text as T

import           Network.HTTP.Client

import           Spotify.Api 

import           System.Environment      (getArgs)

import           AutoPlaylist.Api              
import           Environment

main :: IO ()
main = return () 
{-  case map T.pack rest of
  (searchQuery:searchType:_) -> do 
    let (SpotifyClient searchClient userClient) = makeSpotifyAPIClient 
        searchReq = (searchTracks $ searchClient $ Just $ 
          toHeaderVal authTokResp) (Just searchQuery) (Just searchType) Nothing Nothing Nothing manager spotifyBaseUrl 
    -- | test createPlaylist
    testUserAuth
    testCreatePlaylist "test"
    -- | test search
    -- searchResult <- runExceptT searchReq 
    -- case searchResult of 
    --   Left err -> putStrLn $ show err
    --   Right tracks -> print tracks
  otherwise -> putStrLn invalidArgs 

invalidArgs :: String
invalidArgs = "Error: Please enter two args, <query> <type>."
-}
