{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import           Web.Spock.Shared
import           Web.Spock.Safe

import           Spotify.Api
import           Spotify.Api.Auth

import           System.Environment              (getArgs)

import           AutoPlaylist.Api                      (app)
import           Environment                     (initEnvironment)

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> putStrLn "Usage: auto-playlist-server config.json"
      (confFp:_) -> do
        mEnv <- initEnvironment confFp
        case mEnv of
          Nothing -> return ()
          Just env -> do
            runSpock 3000 $ spock (spockCfg env) app 
  where
    spockCfg env = SpockCfg env PCNoDatabase (defaultSessionCfg ()) Nothing

