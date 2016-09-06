{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Except (ExceptT, runExceptT)

import qualified Data.Text as T
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Spotify.Api 
import           System.Environment  (getArgs)

main :: IO ()
main = do
  args <- map T.pack <$> getArgs
  case args of
    (clientId':clientSecret':_) -> do 
      manager <- newManager tlsManagerSettings
      let clientId = ClientId clientId'
          clientSecret = ClientSecret clientSecret'
          authHeader = Just $ mkAuthHeaderFromCreds (Credentials clientId clientSecret)
          (SpotifyClient searchClient) = makeSpotifyAPIClient 
      res <- runExceptT $ (searchTracks $ searchClient authHeader) (Just "rihanna") (Just "US") Nothing Nothing manager spotifyBaseUrl 
      case res of 
        Left err -> putStrLn $ "Error: " ++ show err
        Right tracks -> print tracks
    _ -> putStrLn "Error: Please enter two args, <clientId> <clientSecret>."
