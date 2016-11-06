{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App (main) where

import           Control.Monad             (void, join, forM)
import           Control.Monad.IO.Class    (liftIO)

import           Data.Aeson                (FromJSON(..))
import qualified Data.Map                  as Map
import           Data.Maybe                (maybe, fromMaybe)
import           Data.Monoid               ((<>))
import           Data.String               (fromString)
import qualified Data.Text                 as T

import           Data.JSString.Text        (textToJSString)
import           GHCJS.Types

import           Reflex
import           Reflex.PerformEvent.Class (performEvent_, TriggerEvent(..))
import           Reflex.Dom 

import           AutoPlaylist.Api              
import           Spotify.Auth.User               as UA
import           Spotify.Types.User              as U 


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
            
          -- | Test if user is logged in 
          (loggedInE, loggedInU) <- newTriggerEvent 
          liftIO $ loggedInU ()
          mIsLoggedInE <- fmap getXhrResponseJSON <$> 
            performRequestAsync (const isLoggedInReq <$> loggedInE)
          
          -- View changes based on result of isLoggedInE, base view is nothing
          widgetHold (return ()) $ mainView <$> mIsLoggedInE

          -- TODO: list playlists

          -- TODO: Create new playlist button

          -- TODO: Add songs to playlist: [ playlist |v] [ "genre" ] [ n ] [ GO ]

-- | Views
---------------
mainView :: forall t m. (DomBuilder t m, MonadWidget t m) => Maybe UA.LoggedIn -> m ()
mainView status = case status of
  Nothing -> notLoggedInView
  Just UA.NotLoggedIn -> notLoggedInView 
  Just (UA.LoggedIn mUser) -> loggedInView mUser

notLoggedInView :: forall t m. (DomBuilder t m, MonadWidget t m) => m ()
notLoggedInView = do
  loginBtnE <- button "Login"
  let loginReqE = fmap (const loginReq) loginBtnE
  loginRespE <- fmap getXhrResponseText <$> 
    performRequestAsync loginReqE
  -- redirect to spotify for authorization
  performEvent_ $ liftIO . setWindowLoc <$> loginRespE

loggedInView :: forall t m. (DomBuilder t m, MonadWidget t m) => Maybe U.User -> m ()
loggedInView mUser = do
  divClass "row" $ divClass "col-xs-12" $
    el "h2" $ text $ "Welcome, " <> maybe "User" u_display_name mUser <> "!"
  divClass "row" $ divClass "col-xs-12" $
    elAttr "img" 
      (Map.singleton "src" "https://cdn.meme.am/instances/43160495.jpg") $ return ()
  divClass "form" $ divClass "form-inline" $ do
    el "label" $ text "Enter Playlist Name:" 
    t <- textInput def
    createBtnE <- button "Create Playlist!"
    let playlistName = _textInput_value t
        createPlaylistReqE = attachDynWith (\plname _ ->
          createPlaylistReq plname) playlistName createBtnE
    createPlaylistRespE <- fmap getXhrResponseText <$> 
      performRequestAsync createPlaylistReqE 
    dynText =<< holdDyn "" createPlaylistRespE  

---------------

loginReq :: XhrRequest () 
loginReq = xhrRequest "GET" "/login" def 

isLoggedInReq :: XhrRequest ()
isLoggedInReq = xhrRequest "GET" "/is-logged-in" def  

createPlaylistReq :: T.Text -> XhrRequest ()
createPlaylistReq = flip (xhrRequest "POST") def . 
  (<>) "/playlist/create/" 

xhrFromURL :: T.Text -> XhrRequest ()  
xhrFromURL url = xhrRequest "GET" url def

getXhrResponseText :: XhrResponse -> T.Text 
getXhrResponseText = fromMaybe "Error" . _xhrResponse_responseText 

getXhrResponseJSON :: FromJSON a => XhrResponse -> Maybe a
getXhrResponseJSON = decodeText . getXhrResponseText

setWindowLoc :: T.Text -> IO ()
setWindowLoc = js_setWindowLoc . textToJSString

foreign import javascript unsafe
  "window.location.replace($1);"
    js_setWindowLoc :: JSString -> IO ()

showJS :: Show a => a -> JSString
showJS = fromString . show
