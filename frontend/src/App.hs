{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App (main) where

import           Control.Monad    (void, forM)
import           AutoPlaylist.Api              

import           Data.Maybe       (fromMaybe)
import qualified Data.Text as T

import           Reflex
import           Reflex.Dom 

main :: IO ()
main =  mainWidget $ void $ do 
  divClass "container" $
    divClass "row" $ 
      divClass "col-md-12" $ 
        divClass "app-container" $ do
          btnEs <- divClass "btn-group" $ 
            fmap leftmost <$> forM [1..9] $ \n -> do
              b <- button $ T.pack $ show n
              return $ const (T.pack $ show n) <$> b 
          el "p" $ dynText =<< holdDyn "" btnEs      
         
          -- TODO: login button that handles auth 
          loginBtnE <- button "Login"
          let loginE = fmap (const loginRequest) loginBtnE
          responseE <- performRequestAsync loginE
          
          el "h4" $ text "Request"
          el "p" $ dynText =<< holdDyn "" (getXhrResponseText <$> responseE)  
          -- TODO: list playlists

          -- TODO: Create new playlist button

          -- TODO: Add songs to playlist: [ playlist |v] [ "genre" ] [ n ] [ GO ]

loginRequest :: XhrRequest ()
loginRequest = xhrRequest "GET" "http://localhost:3000/login" $
  def { _xhrRequestConfig_withCredentials = True } 

getXhrResponseText :: XhrResponse -> T.Text 
getXhrResponseText = fromMaybe "Error" . _xhrResponse_responseText 

