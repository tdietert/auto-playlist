{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AutoPlaylist.Api 
  (app
  ,testUserAuth
  ,testCreatePlaylist
  ) where

import           Web.Spock.Shared
import           Web.Spock.Safe

import           Control.Monad.Trans             (liftIO, MonadIO)
import           Control.Monad.Trans.Except      (runExceptT)
import qualified Control.Monad.STM               as STM
import qualified Control.Concurrent.STM.TVar     as TV

import           Network.HTTP.Client
import           Network.HTTP.Types.Header

import           Data.Aeson                      hiding (json)
import           Data.Coerce                     (coerce)
import           Data.Map                        as Map
import           Data.Monoid                     ((<>))
import qualified Data.Text                       as T

import           Spotify.Api
import           Spotify.Api.Auth.User           as UA
import           Spotify.Api.Auth.Client         as CA
import           Spotify.Types.Playlist          as PL
import           Spotify.Types.User              as U 

import           Environment

app :: SpockM () () Environment ()
app = do
  -- this should probably be the redirURI in the "login" endpoint...
  get root $ do
    (Environment conf userTokensTV spotifyClient manager) <- getState
    code <- param' "code"
    let userAuthReq = UserAuthReq code (redirectUri conf)
    -- authenticate user who was redirected to this endpoint
    liftIO $ putStrLn "Authenticating user with code..."
    eUserAuthResp <- liftIO $ runExceptT $
      userAuthClient userAuthReq (credentials conf) manager clientAuthBaseUrl
    case eUserAuthResp  of
      Left err -> liftIO $ putStrLn $ "Could not authenticate user: " ++ show err
      Right (userAuthResp :: UserAuthResp) -> do
        -- if user is authenticated, set code as cookie
        setCookie "code" code defaultCookieSettings
        liftIO $ STM.atomically $ TV.modifyTVar userTokensTV (insert code userAuthResp)
    file "text/html" $ T.unpack $ redirectFile conf
   
  -- https://developer.spotify.com/web-api/authorization-guide/#authorization-code-flow
  get "login" $ do
    (Environment config _ _ manager) <- getState
    let (Config redirUri _ (Credentials cId _)) = config
        req = UserLoginReq cId redirUri Nothing (Just "playlist-modify-public") Nothing 
    redirect . T.pack . show . getUri $ 
      mkUserLoginRequest req manager userAuthBaseUrl

  post ("playlist" <//> "create" <//> var) $ \name -> do
    liftIO $ putStrLn "Trying to create playlist..."
    withUserAccessToken $ \accTok ->
      withUserClient accTok $ \(UserClient me createPL) -> do 
        manager <- manager <$> getState 
        eUserPriv <- liftIO $ runExceptT $ me manager spotifyBaseUrl
        liftIO $ print eUserPriv 
        case (U.upriv_id <$> eUserPriv) of
          Left err -> json $ T.pack $ show err
          Right userId -> do
            ePlaylist <- liftIO $ runExceptT $ 
              createPL userId (PL.CreatePlaylist name True) manager spotifyBaseUrl  
            case ePlaylist of
              Left err -> json $ T.pack $ show err
              Right pl -> json pl 

-- | API endpoint functions
----------------------------
testUserAuth :: IO ()
testUserAuth = do
  initReq <- parseRequest $ "http://localhost:3000/user-auth"
  let req = initReq { requestHeaders = (hCookie, "code=AQCkCwqjRQ57BDsZIH12KY1mMAskt2gm3NOfdi-SuhzVsyPx0yZN6lCpAaMSlcHqCu7XqHaCItVzSRBgt4NU8DWYxQnqBQcTaMy8-FfZd8LJyY16VoN4UFgPIs-vvOcE3Y6DrGexS_GcmE9GasYCpeq6Y-ICYJNYFQyKDBnkcubSY7jEfGtVFuhTcsj4LdK9qzjlJYPz2ePpzb08CGhsb0lrLA") : requestHeaders initReq }
  manager <- newManager defaultManagerSettings  
  response <- httpLbs req manager
  print $ responseBody response 

testCreatePlaylist :: T.Text -> IO () 
testCreatePlaylist name = do
  initReq <- parseRequest $ "http://localhost:3000/playlist/create/" ++ T.unpack name
  let req = initReq { method = "POST", requestHeaders = (hCookie, "code=AQCkCwqjRQ57BDsZIH12KY1mMAskt2gm3NOfdi-SuhzVsyPx0yZN6lCpAaMSlcHqCu7XqHaCItVzSRBgt4NU8DWYxQnqBQcTaMy8-FfZd8LJyY16VoN4UFgPIs-vvOcE3Y6DrGexS_GcmE9GasYCpeq6Y-ICYJNYFQyKDBnkcubSY7jEfGtVFuhTcsj4LdK9qzjlJYPz2ePpzb08CGhsb0lrLA") : requestHeaders initReq }
  manager <- newManager defaultManagerSettings  
  response <- httpLbs req manager
  print $ responseBody response 

-- | Helpers
--------------
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
      case f . UA.access_token <$> mUserAuthTok of
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
