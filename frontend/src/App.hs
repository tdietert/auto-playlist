{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module App (main) where

import           Control.Monad    (void, forM)
import           AutoPlaylist.Api              

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
              b <- button $ show n
              return $ const (show n) <$> b 
          el "p" $ dynText =<< holdDyn "" btnEs      
         
  -- TODO: login button that handles auth 

  -- TODO: list playlists

  -- TODO: Create new playlist button

  -- TODO: Add songs to playlist: [ playlist |v] [ "genre" ] [ n ] [ GO ]
