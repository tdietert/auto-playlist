{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import qualified Data.Text.Lazy.IO as TL
import Data.Monoid (mconcat)

main = do
  callbackHtml <- TL.readFile "./web/callback.html" 
  scotty 3000 $ do
    get "/callback.html" $ do
      html $ callbackHtml
