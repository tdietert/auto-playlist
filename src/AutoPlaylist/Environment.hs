{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module AutoPlaylist.Environment where

import qualified Control.Monad.STM            as STM
import qualified Control.Concurrent.STM.TVar  as TV
import           Control.Monad.Trans.Except   (runExceptT)

import           Data.Aeson
import           Data.Aeson.Types             (defaultOptions, Options(..))
import qualified Data.ByteString.Lazy.Char8   as BSL
import           Data.Map
import           Data.Monoid                  ((<>))
import qualified Data.Text         as T

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS      (tlsManagerSettings)

import           GHC.Generics (Generic)

import           Spotify.Api
import           Spotify.Auth.Client
import           Spotify.Auth.User

import           System.Directory             (doesFileExist)

data Config = Config
  { spotifyRedirectUri :: RedirectURI
  , redirectFile       :: T.Text
  , spotifyCredentials :: Credentials
  } deriving (Generic, Show)

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
    (RedirectURI <$> v .: "spotifyRedirectUri") <*>
    v .: "redirectFile" <*>
    v .: "spotifyCredentials"

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
  { config :: Config
  , clientAuthToken :: TV.TVar ClientAuthResp
  , userAuthTokens :: TV.TVar (Map T.Text UserAuthResp)
  , spotifyClient :: SpotifyClient
  , spotifyAuthClient :: SpotifyAuthClient
  , manager :: Manager
  }

{- Initializes all necessary mutable states, like client and user auth tokens -}
initEnvironment :: String -> IO (Either T.Text Environment)
initEnvironment configFp = do
  mConf <- readConfig configFp
  case mConf of
    Nothing -> return $ Left "Could not parse config file."
    Just conf -> do
      userAuthToksTV <- STM.atomically $ TV.newTVar empty
      let spotifyClient = mkSpotifyAPIClient
          spotifyAuthClient = mkSpotifyAuthClient
      manager <- newManager tlsManagerSettings
      eAuthTokResp <- runExceptT $
        clientAuthClient (spotifyCredentials conf) manager clientAuthBaseUrl
      case eAuthTokResp of
        Left err -> return $ Left $ T.pack $ show err
        Right authTokResp -> do
          clientAuthTokTV <- STM.atomically $ TV.newTVar authTokResp
          return $ Right $
            Environment conf clientAuthTokTV userAuthToksTV spotifyClient spotifyAuthClient manager


