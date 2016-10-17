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
import qualified Control.Monad.STM               as STM
import qualified Control.Concurrent.STM.TVar     as TV
import           Control.Monad.Trans             (liftIO, MonadIO)
import           Control.Monad.Trans.Except      (runExceptT)

import           Network.HTTP.Client             (getUri)
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Wai                     

import           Data.Aeson                      hiding (json)
import           Data.Bifunctor                  (first)
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Char8           as BSC
import           Data.Coerce                     (coerce)
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid                     ((<>))
import           Data.String                     (fromString, IsString(..))
import qualified Data.Text                       as T

import           Spotify.Api
import           Spotify.Auth.User           as UA
import           Spotify.Auth.Client         as CA
import           Spotify.Types.Playlist          as PL
import           Spotify.Types.User              as U 

import           System.FilePath.Posix           ((</>))

import           Environment

app :: SpockM () () Environment ()
app = do

  middleware corsMiddleware

  -- serve static dir
  get (static "public" <//> var) $ \filename -> do
    file "text/html" $ "public" </> filename

  -- spotify redirects user to this url after authenticated   
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
        liftIO $ STM.atomically $ 
          TV.modifyTVar userTokensTV (Map.insert code userAuthResp)
    redirect $ redirectFile conf
   
  -- https://developer.spotify.com/web-api/authorization-guide/#authorization-code-flow
  -- builds user auth req and redirects to spotify login,
  -- when logged in, user is redirected to 'root'
  get "login" $ do
    (Environment config _ _ manager) <- getState
    let (Config redirUri _ (Credentials cId _)) = config
        req = UserLoginReq cId redirUri Nothing (Just "playlist-modify-public") Nothing 
    redirect . T.pack . show . getUri $ 
      mkUserLoginRequest req manager userAuthBaseUrl

  post ("playlist" <//> "create" <//> var) $ \name -> do
    liftIO $ putStrLn "Trying to create playlist..."
    withUserAccessToken $ \accTok -> do
      withUserClient accTok $ \(UserClient me createPL) -> do 
        manager <- manager <$> getState 
        eUserPriv <- liftIO $ runExceptT $ me manager spotifyBaseUrl
        case (U.u_id <$> eUserPriv) of
          Left err -> do
            liftIO $ putStrLn "Couldn't get user object..."
            text $ T.pack $ show err
          Right userId -> do
            ePlaylist <- liftIO $ runExceptT $ 
              createPL userId (PL.CreatePlaylist name True) manager spotifyBaseUrl  
            case ePlaylist of
              Left err -> do
                liftIO $ putStrLn "Couldn't create playlist..."
                json $ T.pack $ show err
              Right pl -> text $ T.pack $ show pl

  post ("playlist" <//> "build" <//> var <//> var) $ \(genre :: T.Text) (n :: Int) -> do
    liftIO $ putStrLn $ "Adding " ++ show n ++ " songs in genre " ++ T.unpack genre
    text "wut"

-- | Helpers
--------------
withUserAccessToken :: (MonadIO m, HasSpock (ActionCtxT ctx m), Show b,
                       SpockState (ActionCtxT ctx m) ~ Environment) =>
                      (UserAccessToken -> ActionCtxT ctx m b) -> ActionCtxT ctx m () 
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
                  (UserClient -> ActionCtxT ctx m ()) ->
                  ActionCtxT ctx m ()
withUserClient uatok actionWithClient = do
  SpotifyClient _ userClient <- spotifyClient <$> getState
  actionWithClient (userClient $ Just uatok) 

corsMiddleware :: Middleware
corsMiddleware app req respond = 
    app req $ respond . mapResponseHeaders (++ mkCorsHeaders req)
  where
    mkCorsHeaders :: (IsString a, IsString b) => Request -> [(a, b)]
    mkCorsHeaders req = [allowOrigin, allowHeaders, allowMethods, allowCredentials]
      where allowOrigin  = 
              ( fromString "Access-Control-Allow-Origin"
              , fromString . BSC.unpack $
                  fromMaybe "*" . lookup "origin" $ 
                    requestHeaders $ req
              )
            allowHeaders =
              ( fromString "Access-Control-Allow-Headers"
              , fromString "Origin, X-Requested-With, Content-Type, Accept, Authorization"
              )
            allowMethods = 
              ( fromString "Access-Control-Allow-Methods"
              , fromString "GET, POST, PUT, OPTIONS, DELETE"
              )
            allowCredentials = 
              ( fromString "Access-Control-Allow-Credentials"
              , fromString "true"
              )

{- Find better place to put this 
withManagerAndBaseUrl :: (MonadIO m, HasSpock (ActionCtxT ctx m),
                          SpockState (ActionCtxT ctx m) ~ Environment) =>
                          (Manager -> BaseUrl -> ActionCtxT ctx m b) ->
                          ActionCtxT ctx m ()
withManagerAndBaseUrl f = do
  state <- getState
  f (manager state) spotifyBaseUrl
-}
