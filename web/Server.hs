{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import           Control.Monad.Trans.Except   (runExceptT)
import           Web.Spock.Shared
import           Web.Spock.Safe

import           Spotify.Api
import           Spotify.Auth.Client

import           System.Environment           (getArgs)

import           AutoPlaylist.Api             (app)
import           Environment                  (Config(..), Environment(..), initEnvironment)

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> putStrLn "Usage: auto-playlist-server config.json"
      (confFp:_) -> do
        mEnv <- initEnvironment confFp
        case mEnv of
          Nothing -> return ()
          Just env@(Environment conf _ _ manager) -> do
            eAuthTokResp <- runExceptT $ 
              clientAuthClient (credentials conf) manager clientAuthBaseUrl
            case eAuthTokResp of
              Left err -> putStrLn $ "Could not authenticate server: " ++ show err
              Right authTokResp -> runSpock 3000 $ spock (spockCfg env) app 
  where
    spockCfg env = SpockCfg env PCNoDatabase (defaultSessionCfg ()) Nothing

