{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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
import           Data.Bifunctor                  (first,bimap)
import           Data.List                       (foldl')
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromMaybe, catMaybes)
import           Data.Monoid                     ((<>))
import qualified Data.Text                       as T

import           Servant.Client                  (BaseUrl)

import           Spotify.Api                     as Spot
import           Spotify.Auth.User               as UA
import           Spotify.Auth.Client             as CA
import           Spotify.Types.Playlist          as PL
import           Spotify.Types.Track             as ST 
import           Spotify.Types.User              as U 
import           Spotify.Types.PagingObject      as PO 

import           System.FilePath.Posix           ((</>))

import           AutoPlaylist.Environment

app :: SpockM () () Environment ()
app = do

  get root $ file "text/html" "public/index.html"

  -- spotify redirects user to this url after authenticated
  get "callback" $ do
    (Environment conf _ userTokensTV _ manager) <- getState
    
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
    (Environment config _ _ _ manager) <- getState
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
            eUserPriv <- withUserClient $ \(UserClient me _ _) -> 
              first (T.pack . show) <$> spotifyApiCall me
            case eUserPriv of
              Left err -> json $ UA.LoggedIn Nothing
              Right user -> json $ UA.LoggedIn $ Just user

  -- | NOTE (TODO)
  -- |   refresh token could have expired, so need to request new one

  -- check if user has entry in the TVar user map.
  post ("playlist" <//> "build" <//> var <//> var <//> var) $ \plName genre n -> do
    liftIO $ putStrLn "Trying to create playlist..."
    
    -- Create Playlist
    ePlidAndUid <- withUserClient $ \(UserClient me createPL _) -> do 
      eUserPriv <- spotifyApiCall me 
      case (U.u_id <$> eUserPriv) of
        Left err -> return $ Left $ T.pack $ show err 
        Right userId -> bimap (T.pack . show) ((,userId) . PL.pl_id) <$> 
          spotifyApiCall (createPL userId $ PL.CreatePlaylist plName True) 
    
    case ePlidAndUid of
      Left err -> do 
        liftIO $ putStrLn "[Error] Couldn't create playlist:" 
        liftIO $ putStrLn $ show err
        text $ T.pack $ show err
      Right (plId, uId) -> do
        
        liftIO $ putStrLn $ "Created playlist with id: " ++ show plId ++ "!"
     
        -- Search for Songs
        eTracks <- withSearchClient $ \(SearchClient searchTracks) -> spotifyApiCall $ 
          searchTracks (Just genre) (Just "track") Nothing (Just $ min n 50) Nothing
       
        -- Add songs to playlist
        eResp <- withUserClient $ \(UserClient _ _ addTracksPL) -> do
          case eTracks of
            Left err -> return $ Left $ T.pack $ show err 
            Right trackResp -> do
              let tracks = PO.po_items $ Spot.tracks trackResp 
                  trackUris = catMaybes $ map ST.track_uri tracks
              
              -- LOG
              liftIO $ putStrLn "Tracks Found:" 
              forM tracks $ \track -> liftIO $ 
                putStrLn $ T.unpack $ ST.track_name track 
              
              first (T.pack . show) <$>
                spotifyApiCall (addTracksPL uId plId $ Just $ T.intercalate "," trackUris)  
        
        case eResp of
          Left err -> do 
            liftIO $ putStrLn $ "Failed searching tracks: \n" ++ show err
            text $ T.pack $ show err         
          Right resp -> text $ "Succesfully created playlist!"

-- | Helpers
--------------
withUserAccessToken :: (MonadIO m, HasSpock (ActionCtxT ctx m),
                       SpockState (ActionCtxT ctx m) ~ Environment) 
                    => (UserAccessToken -> ActionCtxT ctx m (Either T.Text b)) 
                    -> ActionCtxT ctx m (Either T.Text b)
withUserAccessToken f = do
  mCode <- cookie "code"
  case mCode of 
    Nothing -> return $ Left "Auth Error: no cookie found."
    Just code -> do 
      userAuthToksTV <- userAuthTokens <$> getState
      mUserAuthTok <- liftIO $ Map.lookup code <$> TV.readTVarIO userAuthToksTV
      let mUatok = UA.access_token <$> mUserAuthTok 
      case mUatok of 
        Nothing -> return $ Left $ "User with code: " <> code <> " does not exist." 
        Just uatok -> f uatok -- case match on token expiration here 

withClientAuthTok :: (MonadIO m, HasSpock (ActionCtxT ctx m),
                  SpockState (ActionCtxT ctx m) ~ Environment)
               => (T.Text -> ActionCtxT ctx m b) 
               -> ActionCtxT ctx m b 
withClientAuthTok f = do
  clientAuthTokTV <- clientAuthToken <$> getState
  clientAuthTok <- liftIO $ CA.access_token <$> TV.readTVarIO clientAuthTokTV 
  f clientAuthTok

withUserClient :: (MonadIO m, HasSpock (ActionCtxT ctx m),
                  SpockState (ActionCtxT ctx m) ~ Environment) 
               => (UserClient -> ActionCtxT ctx m (Either T.Text b))
               -> ActionCtxT ctx m (Either T.Text b)
withUserClient actionWithClient = do
  withUserAccessToken $ \uatok -> do
    SpotifyClient _ userClient <- spotifyClient <$> getState
    actionWithClient (userClient $ Just uatok) 

withSearchClient :: (MonadIO m, HasSpock (ActionCtxT ctx m),
                    SpockState (ActionCtxT ctx m) ~ Environment)
                 => (SearchClient -> ActionCtxT ctx m b)
                 -> ActionCtxT ctx m b
withSearchClient actionWithClient = do
  withClientAuthTok $ \clientAuthTok -> do 
    SpotifyClient searchClient _ <- spotifyClient <$> getState
    actionWithClient (searchClient $ Just $ "Bearer " <> clientAuthTok) 

spotifyApiCall  :: (MonadIO m, HasSpock (ActionCtxT ctx m),
                   SpockState (ActionCtxT ctx m) ~ Environment) 
                => (Manager -> BaseUrl -> ExceptT err IO b) 
                -> ActionCtxT ctx m (Either err b) 
spotifyApiCall f = do
  manager <- manager <$> getState
  liftIO $ runExceptT $ 
    f manager spotifyBaseUrl
