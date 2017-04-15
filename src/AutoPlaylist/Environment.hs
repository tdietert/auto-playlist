{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass #-}

module AutoPlaylist.Environment where

import qualified Control.Monad.STM            as STM
import qualified Control.Concurrent.STM.TVar  as TV
import           Control.Monad.Except         (runExceptT)

import           Data.Aeson                   
import           Data.Aeson.Types             (defaultOptions, Options(..))
import qualified Data.ByteString.Lazy.Char8   as BSL
import           Data.Map
import           Data.Monoid                  ((<>))
import qualified Data.Text         as T

import           GHC.Generics (Generic) 

import           Spotify.Api
import           Spotify.Auth.Client
import           Spotify.Auth.User

import           System.Directory             (doesFileExist)

data Config = Config
  { redirectUri  :: RedirectURI 
  , redirectFile  :: T.Text
  , credentials   :: Credentials 
  } deriving (Show, Generic, FromJSON)

readConfig :: FilePath -> IO (Maybe Config)
readConfig fp = do
    isFile <- doesFileExist fp
    eConf <- parseConfig isFile
    case eConf of
      Left err -> putStrLn err >> return Nothing
      Right conf -> return $ Just conf  
  where
    parseConfig :: Bool -> IO (Either String Config)
    parseConfig False = return $ Left $ "Config file " ++ fp ++ " does not exist." 
    parseConfig True  = return . eitherDecode' . BSL.pack =<< readFile fp 

data Environment = Environment 
  { config           :: Config 
  , clientAuthToken  :: TV.TVar ClientAuthResp
  , userAuthTokens   :: TV.TVar (Map T.Text UserAuthResp) 
  , spotifyApiClient :: SpotifyClient
  , spotifyApiEnv    :: SpotifyApiEnv 
  , spotifyAuthEnv   :: SpotifyUserAuthEnv 
  }

{- Initializes all necessary mutable states, like client and user auth tokens -}
initEnvironment :: String -> IO (Either T.Text Environment)
initEnvironment configFp = do
  mConfig <- readConfig configFp
  case mConfig of
    Nothing -> return $ Left "Could not parse config file." 
    Just config -> do
      userAuthToksTV <- STM.atomically $ TV.newTVar empty
      spotifyClientAuthEnv <- mkSpotifyClientAuthEnv
      let clientAuthClient' = clientAuthClient $ credentials config  
      eAuthTokResp <- runSpotifyClientAuthClientM clientAuthClient' spotifyClientAuthEnv 
      case eAuthTokResp of
        Left err -> return $ Left $ T.pack $ show err
        Right authTokResp -> do
          clientAuthTokTV <- STM.atomically $ TV.newTVar authTokResp  
          spotifyApiEnv <- mkSpotifyApiEnv
          spotifyUserAuthEnv <- mkSpotifyUserAuthEnv
          return $ Right $ Environment 
            { config           = config
            , clientAuthToken  = clientAuthTokTV
            , userAuthTokens   = userAuthToksTV
            , spotifyApiClient = mkSpotifyAPIClient  
            , spotifyApiEnv    = spotifyApiEnv
            , spotifyAuthEnv   = spotifyUserAuthEnv
            } 



