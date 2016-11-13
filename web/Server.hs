{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import           Control.Monad

import qualified Data.Text                  as T

import           System.Environment         (getArgs)

import           Web.Spock.Shared
import           Web.Spock.Safe

import           AutoPlaylist.Api           (app)
import           AutoPlaylist.Environment   (Config(..), Environment(..), initEnvironment)
import           Server.Utils               (staticMiddleware)
import           Spotify.Api
import           Spotify.Auth.Client

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: auto-playlist-server config.json"
    (confFp:_) -> do
      eEnv <- initEnvironment confFp
      case eEnv of
        Left err -> putStrLn $ T.unpack err
        Right env@(Environment conf _ _ _ manager) -> do
          let spockCfg = SpockCfg env PCNoDatabase (defaultSessionCfg ()) Nothing
          runSpock 3000 $ spock spockCfg $ do
            middleware staticMiddleware 
            app 
