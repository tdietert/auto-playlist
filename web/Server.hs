{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import           Web.Spock.Shared
import           Web.Spock.Safe

import           Control.Monad.Trans             (liftIO, MonadIO)
import           Control.Monad.Trans.Except      (runExceptT)
import qualified Control.Monad.STM               as STM
import qualified Control.Concurrent.STM.TVar     as TV

import           Network.HTTP.Client

import           Data.Coerce                     (coerce)
import           Data.Map                        as Map
import           Data.Monoid                     ((<>))
import qualified Data.Text                       as T

import           Spotify.Api
import           Spotify.Api.Auth
import           Spotify.Types.Playlist          as PL
import           Spotify.Types.User              as U 

import           System.Environment              (getArgs)

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
    (Environment conf userTokensTV spotifyClient manager) <- getState
    code <- param' "code"
    liftIO $ print code 
    let userAuthReq = UserAuthReq code (redirectUri conf)
    -- authenticate user who was redirected to this endpoint
    eUserAuthResp <- liftIO $ runExceptT $
      userAuthClient userAuthReq (credentials conf) manager clientAuthBaseUrl
    case eUserAuthResp of
      Left err -> liftIO $ putStrLn $ "Could not authenticate user:" ++ show err
      Right userAuthResp -> do
        -- if user is authenticated, set code as cookie
        setCookie "code" code defaultCookieSettings
        liftIO $ STM.atomically $ TV.modifyTVar userTokensTV (insert code userAuthResp)
    file "text/html" $ T.unpack $ redirectFile conf

  get "user-auth" $ do
    mCode <- cookie "code"
    case mCode of 
      Nothing -> text "Auth Error: no cookie found."
      Just code -> do 
        userAuthToksTV <- userAuthTokens <$> getState
        userAuthToks  <- liftIO $ TV.readTVarIO userAuthToksTV
        case Map.lookup code userAuthToks of
          Nothing -> liftIO $ putStrLn $ 
            "User with code: " <> T.unpack code <> " does not exist." 
          Just authTok -> text $ coerce $ uaresp_access_token authTok

  subcomponent "playlist" $ do
    post ("create" <//> var) $ \name -> do
      withUserAccessToken $ \accTok ->
       withUserClient accTok $ \(UserClient me createPL) -> do 
         manager <- manager <$> getState 
         eUserId <- liftIO $ runExceptT $ me manager spotifyBaseUrl
         case (U.upriv_id <$> eUserId) of
           Left err -> text $ T.pack $ show err
           Right userId -> do
             ePlaylist <- liftIO $ runExceptT $ 
               createPL userId (PL.CreatePlaylist name True) manager spotifyBaseUrl  
             case ePlaylist of
               Left err -> text $ T.pack $ show err
               Right pl -> text $ T.pack $ show pl 

withUserAccessToken :: (MonadIO m, HasSpock (ActionCtxT ctx m),
                       SpockState (ActionCtxT ctx m) ~ Environment) =>
                      (UserAccessToken -> ActionCtxT ctx m b) -> ActionCtxT ctx m () 
withUserAccessToken f = do
  mCode <- cookie "code"
  case mCode of 
    Nothing -> text "Auth Error: no cookie found."
    Just code -> do 
      userAuthToksTV <- userAuthTokens <$> getState
      mUserAuthTok <- liftIO $ Map.lookup code <$> TV.readTVarIO userAuthToksTV
      case f . uaresp_access_token <$> mUserAuthTok of
        Nothing -> liftIO $ putStrLn $
            "User with code: " <> T.unpack code <> " does not exist." 
        Just res -> return ()

withUserClient :: (MonadIO m, HasSpock (ActionCtxT ctx m),
                  SpockState (ActionCtxT ctx m) ~ Environment) =>
                  UserAccessToken -> 
                  (UserClient -> ActionCtxT ctx m ()) ->
                  ActionCtxT ctx m ()
withUserClient uatok actionWithClient = do
  SpotifyClient _ userClient <- spotifyClient <$> getState
  actionWithClient (userClient $ Just uatok) 

{- Find better place to put this 
withManagerAndBaseUrl :: (MonadIO m, HasSpock (ActionCtxT ctx m),
                          SpockState (ActionCtxT ctx m) ~ Environment) =>
                          (Manager -> BaseUrl -> ActionCtxT ctx m b) ->
                          ActionCtxT ctx m ()
withManagerAndBaseUrl f = do
  state <- getState
  f (manager state) spotifyBaseUrl
-}
