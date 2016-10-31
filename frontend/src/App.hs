{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App (main) where

import           Control.Monad    (void, forM)
import           Control.Monad.IO.Class (liftIO)
import           AutoPlaylist.Api              

import qualified Data.Map         as Map
import           Data.Maybe       (fromMaybe)
import           Data.Monoid      ((<>))
import           Data.String      (fromString)
import qualified Data.Text as T

import           Data.JSString.Text (textToJSString)
import           GHCJS.Types

import           Reflex
import           Reflex.PerformEvent.Class (performEvent_)
import           Reflex.Dom 

main :: IO ()
main =  mainWidget $ void $ do 
  divClass "container" $
    divClass "row" $ 
      divClass "col-md-12" $ 
        divClass "app-container" $ do
          
          {- Example: 
          btnEs <- divClass "btn-group" $ 
            fmap leftmost <$> forM [1..9] $ \n -> do
              b <- button $ T.pack $ show n
              return $ const (T.pack $ show n) <$> b 
          el "p" $ dynText =<< holdDyn "0" btnEs      
          -}

          -- TODO: login button that handles auth 
          loginBtnE <- button "Login"
          let loginReqE = fmap (const loginRequest) loginBtnE
          loginRespE <- fmap getXhrResponseText <$> 
            performRequestAsync loginReqE
          -- redirect to spotify for authorization
          performEvent_ $ liftIO . setWindowLoc <$> loginRespE
          -- | TODO: make loginRequest happen on page load!

          -- TODO: list playlists

          -- TODO: Create new playlist button

          -- TODO: Add songs to playlist: [ playlist |v] [ "genre" ] [ n ] [ GO ]

loginRequest :: XhrRequest () 
loginRequest = xhrRequest "GET" "/login" def 

xhrFromURL :: T.Text -> XhrRequest ()  
xhrFromURL url = xhrRequest "GET" url def

getXhrResponseText :: XhrResponse -> T.Text 
getXhrResponseText = fromMaybe "Error" . _xhrResponse_responseText 

setWindowLoc :: T.Text -> IO ()
setWindowLoc = js_setWindowLoc . textToJSString

foreign import javascript unsafe
  "window.location.replace($1);"
    js_setWindowLoc :: JSString -> IO ()

showJS :: Show a => a -> JSString
showJS = fromString . show
