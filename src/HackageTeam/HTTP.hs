module HackageTeam.HTTP
  ( module Network.HTTP.Simple
  , addApiKeyAuthorization
  , disableRedirects
  , lookupResponseHeader
  ) where

import HackageTeam.Prelude

import Network.HTTP.Client (Request(..))
import Network.HTTP.Simple
import Network.HTTP.Types.Header (HeaderName, hAuthorization)

addApiKeyAuthorization :: ByteString -> Request -> Request
addApiKeyAuthorization bs = addRequestHeader hAuthorization $ "X-ApiKey " <> bs

disableRedirects :: Request -> Request
disableRedirects req = req { redirectCount = 0 }

lookupResponseHeader :: HeaderName -> Response body -> Maybe Text
lookupResponseHeader n resp = case getResponseHeader n resp of
  (bs : _) -> Just $ decodeUtf8With lenientDecode bs
  _ -> Nothing
