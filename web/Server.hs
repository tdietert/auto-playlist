{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import           Control.Monad

import qualified Data.Text                  as T


import           Web.Spock
import           Web.Spock.Config

import           AutoPlaylist.Api           (app)
import           AutoPlaylist.Environment   (Config(..), Environment(..), initEnvironment)
import           Server.Utils               (staticMiddleware)
import           Spotify.Api
import           Spotify.Auth.Client

import           System.Environment         (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: auto-playlist-server config.json"
    (confFp:_) -> do
      eEnv <- initEnvironment confFp
      case eEnv of
        Left err -> putStrLn $ T.unpack err
        Right env -> do
          spockCfg <- defaultSpockCfg () PCNoDatabase env
          runSpock 3000 $ spock spockCfg $ middleware staticMiddleware >> app 
