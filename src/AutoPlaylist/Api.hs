{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AutoPlaylist.Api 
  ( app
  ) where

import           Web.Spock.Shared
import           Web.Spock.Safe

import           Control.Monad                   (void)
import           Control.Monad.Except
import qualified Control.Monad.STM               as STM
import qualified Control.Concurrent.STM.TVar     as TV
import           Control.Monad.Trans             (liftIO, MonadIO)
import           Control.Monad.Trans.Except      (runExceptT)

import           Network.HTTP.Client             (getUri, Manager)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status

import           Data.Aeson                      hiding (json)
import           Data.Bifunctor                  (first)
import           Data.List                       (foldl')
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid                     ((<>))
import qualified Data.Text                       as T

import           Servant.Client                  (BaseUrl)

import           Spotify.Api
import           Spotify.Auth.User               as UA
import           Spotify.Auth.Client             as CA
import           Spotify.Types.Playlist          as PL
import           Spotify.Types.User              as U 

import           System.FilePath.Posix           ((</>))

import           Environment

app :: SpockM () () Environment ()
app = do

  get root $ file "text/html" "public/index.html"

  -- spotify redirects user to this url after authenticated
  get "callback" $ do
    (Environment conf userTokensTV spotifyClient manager) <- getState
    
    -- | Note:
    -- |   spotify could respond with `error` and `state` query params
    -- |   as well, and in this case, we should do something?
    code <- param' "code"
  
    let userAuthReq = UserAuthReq code (redirectUri conf)
    -- authenticate user who was redirected to this endpoint
    liftIO $ putStrLn "Authenticating user with code..."
    eUserAuthResp <- liftIO $ runExceptT $
      userAuthClient userAuthReq (credentials conf) manager clientAuthBaseUrl
    case eUserAuthResp of
      Left err -> liftIO $ putStrLn $ "Could not authenticate user: " ++ show err
      Right (userAuthResp :: UserAuthResp) -> do
        -- if user is authenticated, set code as cookie
        setCookie "code" code defaultCookieSettings
        liftIO $ STM.atomically $ 
          TV.modifyTVar userTokensTV (Map.insert code userAuthResp)
        -- file "text/html" "public/index.html"
        redirect "/" 

  -- https://developer.spotify.com/web-api/authorization-guide/#authorization-code-flow
  -- builds user auth req and redirects to spotify login,
  -- when logged in, user is redirected to 'root'
  get "login" $ do
    (Environment config _ _ manager) <- getState
    let (Config redirUri _ (Credentials cId _)) = config
        req = UserLoginReq cId redirUri Nothing (Just "playlist-modify-public") Nothing 
    let authURL = T.pack . show . getUri $ 
          mkUserLoginRequest req manager userAuthBaseUrl
    -- return url for client to use
    text authURL

  get "is-logged-in" $ do
    liftIO $ putStrLn "Checking if user is logged in..."
    mCode <- cookie "code"
    case mCode of
      Nothing -> json UA.NotLoggedIn 
      Just code -> do
        userAuthToksTV <- userAuthTokens <$> getState
        mUserAuthTok <- liftIO $ Map.lookup code <$> 
          TV.readTVarIO userAuthToksTV
        case mUserAuthTok of
          Nothing -> json UA.NotLoggedIn
          Just _ -> do
            withUserAccessToken $ \accTok -> 
              withUserClient accTok $ \(UserClient me createPL) -> do 
                eUserPriv <- spotifyApiCall me
                case eUserPriv of
                  Left err -> json $ UA.LoggedIn Nothing
                  Right user -> json $ UA.LoggedIn $ Just user 
    -- | NOTE (TODO)
    -- |   refresh token could have expired, so need to request new one
  
    -- check if user has entry in the TVar user map.
   
  post ("playlist" <//> "create" <//> var) $ \name -> do
    liftIO $ putStrLn "Trying to create playlist..."
    withUserAccessToken $ \accTok -> 
      withUserClient accTok $ \(UserClient me createPL) -> do 
        eUserPriv <- spotifyApiCall me 
        case (U.u_id <$> eUserPriv) of
          Left err -> do
            liftIO $ putStrLn "Couldn't get user object..."
            text $ T.pack $ show err
          Right userId -> do
            ePlaylist <- spotifyApiCall $ 
              createPL userId (PL.CreatePlaylist name True) 
            case ePlaylist of
              Left err -> do
                liftIO $ putStrLn "Couldn't create playlist..."
                text $ T.pack $ show err
              Right pl -> text $ T.pack $ 
                "Created playlist: " ++ show (PL.pl_name pl) ++ "!"

  post ("playlist" <//> "build" <//> var <//> var) $ \(genre :: T.Text) (n :: Int) -> do
    liftIO $ putStrLn $ "Adding " ++ show n ++ " songs in genre " ++ T.unpack genre
    text "wut"

  
-- | Helpers
--------------
withUserAccessToken :: (MonadIO m, HasSpock (ActionCtxT ctx m),
                       SpockState (ActionCtxT ctx m) ~ Environment) =>
                      (UserAccessToken -> ActionCtxT ctx m ()) -> ActionCtxT ctx m () 
withUserAccessToken f = do
  mCode <- cookie "code"
  case mCode of 
    Nothing -> liftIO $ putStrLn "Auth Error: no cookie found."
    Just code -> do 
      userAuthToksTV <- userAuthTokens <$> getState
      mUserAuthTok <- liftIO $ Map.lookup code <$> TV.readTVarIO userAuthToksTV
      let mUatok = UA.access_token <$> mUserAuthTok 
      case mUatok of 
        Nothing -> liftIO $ putStrLn $
            "User with code: " <> T.unpack code <> " does not exist." 
        Just uatok -> void $ f uatok -- case match on token expiration here 

withUserClient :: (MonadIO m, HasSpock (ActionCtxT ctx m),
                  SpockState (ActionCtxT ctx m) ~ Environment) =>
                  UserAccessToken -> 
                  (UserClient -> ActionCtxT ctx m b) ->
                  ActionCtxT ctx m b
withUserClient uatok actionWithClient = do
  SpotifyClient _ userClient <- spotifyClient <$> getState
  actionWithClient (userClient $ Just uatok) 

spotifyApiCall  :: (MonadIO m, HasSpock (ActionCtxT ctx m),
                          SpockState (ActionCtxT ctx m) ~ Environment) =>
                          (Manager -> BaseUrl -> ExceptT err IO b) ->
                          ActionCtxT ctx m (Either err b) 
spotifyApiCall f = do
  manager <- manager <$> getState
  liftIO $ runExceptT $ 
    f manager spotifyBaseUrl
