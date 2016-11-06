{-# LANGUAGE OverloadedStrings #-}

module Server.Utils where

import qualified Data.ByteString.Char8         as BSC
import           Data.Maybe                    (fromMaybe)
import           Data.String                   (fromString, IsString(..))

import           Network.Wai                     
import qualified Network.Wai.Middleware.Static as MWS

import           Web.Spock.Safe

staticMiddleware :: Middleware
staticMiddleware = MWS.static

corsMiddleware :: Middleware
corsMiddleware app req respond = 
    app req $ respond . mapResponseHeaders (++ mkCorsHeaders req)
  where
    mkCorsHeaders :: (IsString a, IsString b) => Request -> [(a, b)]
    mkCorsHeaders req = [allowOrigin, allowHeaders, allowMethods, allowCredentials]
      where allowOrigin  = 
              ( fromString "Access-Control-Allow-Origin"
              , fromString . BSC.unpack $
                  fromMaybe "*" $ lookup "origin" $ 
                    requestHeaders req             
              )
            allowHeaders =
              ( fromString "Access-Control-Allow-Headers"
              , fromString "Origin, Cache-Control, X-Requested-With, Content-Type, Accept, Authorization"
              )
            allowMethods = 
              ( fromString "Access-Control-Allow-Methods"
              , fromString "GET, POST, PUT, OPTIONS, DELETE"
              )
            allowCredentials = 
              ( fromString "Access-Control-Allow-Credentials"
              , fromString "true"
              )
