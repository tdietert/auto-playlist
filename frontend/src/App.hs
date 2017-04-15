{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App (main) where

import           Control.Monad             (void, join, forM)
import           Control.Monad.IO.Class    (liftIO)

import           Data.Aeson                (FromJSON(..))
import           Data.List                 (foldl')
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
  divRowCol12 $ el "h2" $ 
    text $ "Welcome, " <> maybe "User" u_display_name mUser <> "!"
  divRowCol12 $ elAttr "img" 
    (Map.singleton "src" "https://cdn.meme.am/instances/43160495.jpg") $ return ()
  divClass "form" $ do
    
    playlistName <- formGroup $ 
      formRowWithLabel "Playlist Name:" $
        _textInput_value <$> textInput def
   
    genre <- formGroup $ 
      formRowWithLabel "Genre:" $
        _textInput_value <$> textInput def
    
    nSongs <- formGroup $
      formRowWithLabel "Number of Songs:" $ do
        let numMap = foldl' (\nmap n -> Map.insert n (T.pack $ show n) nmap) Map.empty [1..50]
        _dropdown_value <$> dropdown (1 :: Int) (constDyn numMap) def

    createBtnE <- button "Create Playlist!"
    let plReqArgs = (,,) <$> playlistName <*> genre <*> nSongs
        createPlaylistReqE = attachDynWith 
          (\(plName,genre,n)-> const $ createPlaylistReq plName genre n) plReqArgs createBtnE
    
    createPlaylistRespE <- fmap getXhrResponseText <$> 
      performRequestAsync createPlaylistReqE 
    dynText =<< holdDyn "" createPlaylistRespE  

{- DOM Helpers -}
divRow :: forall t m a. DomBuilder t m => m a -> m a
divRow = divClass "row" 

divCol :: forall t m a. DomBuilder t m => Int -> m a -> m a
divCol x = divClass ("col-xs-" <> T.pack (show x))

divRowCol12 :: forall t m a. DomBuilder t m => m a -> m a
divRowCol12 = divClass "row" . divClass "col-xs-12"

formGroup :: forall t m a. DomBuilder t m => m a -> m a
formGroup = divClass "form-group row" 

formRowWithLabel :: forall t m a. DomBuilder t m => T.Text -> m a -> m a
formRowWithLabel lbl input = do 
  elClass "label" "col-xs-3 col-form-label" $ text lbl
  divCol 9 input

{- XHR Helpers -} 
loginReq :: XhrRequest () 
loginReq = xhrRequest "GET" "/login" def 

isLoggedInReq :: XhrRequest ()
isLoggedInReq = xhrRequest "GET" "/is-logged-in" def  

createPlaylistReq :: T.Text -> T.Text -> Int -> XhrRequest ()
createPlaylistReq plName genre n = xhrRequest "POST"
  ("playlist/build/" <> plName <> "/" <> genre <> "/" <> T.pack (show n)) def 

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
