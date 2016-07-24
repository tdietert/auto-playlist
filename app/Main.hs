module Main where

import Control.Monad.Trans.Except (ExceptT, runExceptT)

import qualified Data.Text as T
import           Network.HTTP.Client (newManager, defaultManagerSettings)
import           Soundcloud.Api 
import           System.Environment  (getArgs)

main :: IO ()
main = do
  (clientId:g:genres) <- map T.pack <$> getArgs
  manager <- newManager defaultManagerSettings
  let (SoundcloudClient trackClient) = makeAPIClient
      trackAPI = trackClient (Just clientId)
  res <- runExceptT $ (searchTracksByGenre trackAPI) (g:genres) manager scApiBaseURL
  case res of 
    Left err -> putStrLn $ "Error: " ++ show err
    Right tracks -> mapM_ print tracks
