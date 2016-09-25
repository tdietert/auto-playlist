{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Except        (runExceptT)

import qualified Data.Text as T

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           Spotify.Api 
import           Spotify.Api.Auth

import           System.Environment      (getArgs)

import           Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    (confFp:rest) -> do
      mConf <- readConfig confFp
      case mConf of
        Nothing -> return ()
        Just (Config _ _ creds) -> do
          manager <- newManager tlsManagerSettings
          -- First authorize client app
          eAuthTokResp <- runExceptT $ clientAuthClient creds manager clientAuthBaseUrl
          case eAuthTokResp of 
            Left err -> putStrLn $ "Could not authenticate client: " ++ show err
            Right authTokResp -> case map T.pack rest of
                (searchQuery:searchType:_) -> do 
                  -- Next, run test search
                  let (SpotifyClient searchClient userClient) = makeSpotifyAPIClient 
                      searchReq = (searchTracks $ searchClient $ Just $ 
                        toHeaderVal authTokResp) (Just searchQuery) (Just searchType) Nothing Nothing Nothing manager spotifyBaseUrl 
                  searchResult <- runExceptT searchReq 
                  case searchResult of 
                    Left err -> putStrLn $ show err
                    Right tracks -> print tracks
                otherwise -> putStrLn invalidArgs 
    otherwise -> putStrLn invalidArgs

invalidArgs :: String
invalidArgs = "Error: Please enter two args, <clientId> <clientSecret> <query> <type>."
