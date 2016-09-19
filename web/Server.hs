{-# LANGUAGE OverloadedStrings #-}

import           Web.Spock.Shared
import           Web.Spock.Safe

import           Control.Monad.Trans             (liftIO)
import           Control.Monad.Trans.Except      (runExceptT)
import qualified Control.Monad.STM               as STM
import qualified Control.Concurrent.STM.TVar     as TV

import           Network.HTTP.Client

import           Data.Coerce                     (coerce)
import           Data.Map 
import qualified Data.Text         as T

import           Spotify.Api
import           Spotify.Api.Auth

import           System.Environment    (getArgs)

import           Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> putStrLn "Usage: auto-playlist-server config.json"
      (confFp:_) -> do
        -- init env
        mEnv <- initEnvironment confFp
        case mEnv of
          Nothing -> return ()
          Just env -> do
            runSpock 3000 $ spock (spockCfg env) app 
  where
    spockCfg env = SpockCfg env PCNoDatabase (defaultSessionCfg ()) Nothing

app :: SpockM () () Environment ()
app = do
  get root $ do
    (Environment conf userTokensTV manager) <- getState
    code <- param' "code"
    liftIO $ print code 
    
    let userAuthReq = UserAuthReq code (redirectUri conf)
    -- authenticate user who was redirected to this endpoint
    eUserAuthResp <- liftIO $ runExceptT $
      userAuthClient userAuthReq (credentials conf) manager userAuthBaseUrl
    case eUserAuthResp of
      Left err -> liftIO $ putStrLn "Could not authenticate user."
      Right userAuthResp -> do
        -- if user is authenticated, set code as cookie
        setCookie "code" code defaultCookieSettings
        liftIO $ STM.atomically $ TV.modifyTVar userTokensTV (insert code userAuthResp)
    file "text/html" $ T.unpack $ coerce $ redirectUri conf


