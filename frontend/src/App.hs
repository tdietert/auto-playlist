{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module App (main) where

import           Control.Applicative
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
import           Reflex.PerformEvent.Class (performEvent_)
import           Reflex.Dom

import           AutoPlaylist.Api
import           Spotify.Auth.User               as UA
import           Spotify.Types.User              as U
import           Spotify.Types.Track             as ST


main :: IO ()
main =  mainWidget $ void $ do
  divClass "container" $
    divClass "row" $
      divClass "col-md-12" $
        divClass "app-container" $ do
          -- | Test if user is logged in
          (loggedInE, loggedInU) <- newTriggerEvent
          liftIO $ loggedInU ()
          mIsLoggedInE <- fmap decodeXhrResponse <$>
            performRequestAsync (const isLoggedInReq <$> loggedInE)

          -- View changes based on result of isLoggedInE, base view is nothing
          widgetHold (return ()) $ mainView <$> mIsLoggedInE

-- | Views
---------------
mainView :: forall t m. (DomBuilder t m, MonadWidget t m) => Maybe UA.LoggedIn -> m ()
mainView status = case status of
  Nothing -> notLoggedInView
  Just UA.NotLoggedIn -> notLoggedInView
  Just (UA.LoggedIn mUser) -> loggedInView mUser

notLoggedInView :: forall t m. (DomBuilder t m, MonadWidget t m) => m ()
notLoggedInView = do
  loginBtnE <- do
    let btnAttrs = Map.fromList
          [("type","button")
          ,("class","btn btn-success btn-block")
          ,("id","login-button")
          ]
    (e, _) <- elAttr "div" (Map.singleton "style" "text-align:center") $
      elAttr' "button" btnAttrs $ text "Login with Spotify!"
    return $ domEvent Click e

  let loginReqE = fmap (const loginReq) loginBtnE
  loginRespE <- fmap getXhrResponseText <$> performRequestAsync loginReqE

  -- redirect to spotify for authorization
  performEvent_ $ flip fmap loginRespE $ \mResp -> do
    case mResp of
      Just loginUrl -> liftIO $ setWindowLoc loginUrl
      Nothing -> return ()

loggedInView :: forall t m. (DomBuilder t m, MonadWidget t m) => Maybe U.User -> m ()
loggedInView mUser = do

  divRowCol12 $ el "h2" $
    text $ "Welcome, " <> maybe "User" u_display_name mUser <> "!"

  divRowCol12 $ el "form" $ do

    (searchQ, searchBtnE) <- formGroup $ divClass "input-group" $ do
      searchText <- liftA _textInput_value $ textInput $
        def { _textInputConfig_attributes = constDyn $ Map.singleton "class" "form-control" }
      searchBtnE <- elClass "span" "input-group-btn" $ buttonClass "Search" "btn btn-default"

      return (searchText, searchBtnE)

    let searchReqE = attachDynWith
          (\searchQ' -> const $ spotifySearchReq searchQ') searchQ searchBtnE

    searchRespE <- fmap decodeXhrResponse <$> performRequestAsync searchReqE :: m (Event t (Maybe [ST.Track]))
    -- Event t (Maybe [Track])
    void $ widgetHold (return ()) $ flip fmap searchRespE $ \mTracks ->
      case mTracks of
        Nothing -> return ()
        Just tracks -> elAttrs "table" [("style","border-spacing:10px;")] $
          mapM_ trackWidget tracks

{- DOM Helpers -}
elAttrs :: (DomBuilder t m) => T.Text -> [(T.Text,T.Text)] -> m () -> m ()
elAttrs tag attrs = elAttr tag (Map.fromList attrs)

trackWidget :: (DomBuilder t m) => ST.Track -> m ()
trackWidget ST.Track{..} = do
  el "tr" $ mapM_ (el "th" . text) [ "Title", "Preview Url" ]
  el "tr" $ mapM_ (el "td")
    [ text track_name
    , maybe (text "No track preview.")
        (\url -> elAttrs "a" [("href", url), ("target","blank_")] $ text url)
        track_preview_url
    ]

buttonClass :: DomBuilder t m => T.Text -> T.Text -> m (Event t ())
buttonClass t class_ = do
  (e,_) <- elAttr' "button" (Map.insert "class" class_ $ Map.singleton "type" "button") $ text t
  return $ domEvent Click e

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

spotifySearchReq :: T.Text -> XhrRequest ()
spotifySearchReq query = xhrRequest "POST" ("spotify/search/" <> query) def

xhrFromURL :: T.Text -> XhrRequest ()
xhrFromURL url = xhrRequest "GET" url def

getXhrResponseText :: XhrResponse -> Maybe T.Text
getXhrResponseText = _xhrResponse_responseText

setWindowLoc :: T.Text -> IO ()
setWindowLoc = js_setWindowLoc . textToJSString

foreign import javascript unsafe
  "window.location.replace($1);"
    js_setWindowLoc :: JSString -> IO ()

showJS :: Show a => a -> JSString
showJS = fromString . show
