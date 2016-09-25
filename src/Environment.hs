{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Environment where

import qualified Control.Monad.STM               as STM
import qualified Control.Concurrent.STM.TVar     as TV

import           Data.Aeson                     
import           Data.Aeson.Types                (defaultOptions, Options(..))
import qualified Data.ByteString.Lazy.Char8      as BSL
import           Data.Map
import           Data.Monoid                     ((<>))
import qualified Data.Text         as T

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS         (tlsManagerSettings)

import           GHC.Generics (Generic) 

import           Spotify.Api
import           Spotify.Api.Auth

import           System.Directory      (doesFileExist)

data Config = Config
  { redirectUri  :: RedirectURI 
  , redirectFile  :: T.Text
  , credentials   :: Credentials 
  } deriving (Generic, Show)

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
    (RedirectURI <$> v .: "redirectUri") <*>
    v .: "redirectFile" <*>
    v .: "credentials" 

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
  , userAuthTokens :: TV.TVar (Map T.Text UserAuthResp) 
  , spotifyClient :: SpotifyClient
  , manager :: Manager
  }

initEnvironment :: String -> IO (Maybe Environment)
initEnvironment configFp = do
  mConf <- readConfig configFp
  case mConf of
    Nothing -> return Nothing
    Just conf -> do
      state <- STM.atomically $ TV.newTVar empty
      let spotifyClient = makeSpotifyAPIClient
      manager <- newManager tlsManagerSettings
      return $ Just $ Environment conf state spotifyClient manager
  

