{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spotify.Auth.User where

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Except

import           Data.Aeson              hiding (encode)
import           Data.Aeson.Types        (Options(..), defaultOptions)
import           Data.Bifunctor          (second)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteString.Char8   as BSC
import           Data.ByteString.Base64  (encode)
import           Data.Coerce             (coerce)
import           Data.List               (intercalate, concat)
import           Data.Monoid             ((<>))
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T

import           GHC.Generics            (Generic)

import           Network.HTTP.Client
import           Network.HTTP.Types      hiding (Header)
import           Network.HTTP.Media

import           Web.HttpApiData

import           Servant.API.ContentTypes  (ToFormUrlEncoded(..))
import           Servant.Client            hiding (responseBody)
import           Spotify.Auth.Client
import qualified Spotify.Types.User         as U

data LoggedIn = LoggedIn (Maybe U.User) | NotLoggedIn
  deriving (Generic, ToJSON, FromJSON)

newtype UserAccessToken = UserAccessToken T.Text
  deriving (Show, Generic)

instance ToJSON UserAccessToken where
  toJSON (UserAccessToken uatok) = toJSON uatok
instance FromJSON UserAccessToken where
  parseJSON = fmap UserAccessToken . parseJSON

instance ToHttpApiData UserAccessToken where
  toUrlPiece = coerce
  toQueryParam = coerce
  toHeader (UserAccessToken uatok) = "Bearer " <> T.encodeUtf8 uatok

-- no prefixes on fields because
-- querying Spotify API spec in Spotify/Api.hs
-- is local and decodes JSON from Spotify server,
-- so 2 prefixes are added: uaresp_uaresp_access_token.
data UserAuthResp = UserAuthResp
  { access_token  :: UserAccessToken
  , token_type    :: T.Text
  , expires_in    :: Int
  , refresh_token :: T.Text
  , scope         :: T.Text
  } deriving (Generic, Show, FromJSON)

newtype RedirectURI = RedirectURI T.Text
  deriving (Generic, Show, ToJSON, FromJSON)

newtype Code = Code T.Text
  deriving (Generic, Show, ToJSON, FromJSON)

data UserAuthReq = UserAuthReq
  { grant_type   :: T.Text
  , code         :: T.Text
  , redirect_uri :: T.Text
  } deriving (Generic, Show)

-- | TODO use generics to get field names
instance ToFormUrlEncoded UserAuthReq where
  toFormUrlEncoded (UserAuthReq gt c ru) =
    zip ["grant_type","code","redirect_uri"] [gt,c,ru]

mkUserAuthReq :: T.Text -> Code -> RedirectURI -> UserAuthReq
mkUserAuthReq grantType (Code code) (RedirectURI redirUri) =
  UserAuthReq grantType code redirUri
