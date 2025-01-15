{-# LANGUAGE UndecidableInstances #-}

-- | An implementation of 'MonadHackage' that performs real HTTP
module HackageTeam.HackageApi.Actual
  ( HackageApiKey (..)
  , HasHackageApiKey (..)
  , ActualHackage (..)
  ) where

import HackageTeam.Prelude

import Data.Aeson
import HackageTeam.HTTP
import HackageTeam.HackageApi
import Network.HTTP.Types.Header (hLocation)
import RIO.Text (stripPrefix, stripSuffix)

newtype HackageApiKey = HackageApiKey
  { unwrap :: ByteString
  }

unL :: Lens' HackageApiKey ByteString
unL = lens (.unwrap) $ \x y -> x {unwrap = y}

class HasHackageApiKey env where
  hackageApiKeyL :: Lens' env HackageApiKey

instance HasHackageApiKey HackageApiKey where
  hackageApiKeyL = id

newtype HackageUser = HackageUser
  { groups :: [Text]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

packageFromMaintainers :: Text -> Maybe Package
packageFromMaintainers a = do
  b <- stripPrefix "/package/" a
  c <- stripSuffix "/maintainers" b
  pure $ Package c

newtype HackageGroup = HackageGroup
  { members :: [HackageGroupMember]
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype HackageGroupMember = HackageGroupMember
  { username :: HackageUsername
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

newtype ActualHackage m a = ActualHackage
  { unActualHackage :: m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadLogger
    , MonadReader env
    )

newtype GetSelfError = GetSelfError (Response ())
  deriving stock (Show)

instance Exception GetSelfError where
  displayException (GetSelfError resp) =
    unlines
      [ "Unable to determine Hackage Username"
      , " Invalid /users/account-management redirect:" <> show resp
      , " Unable to parse Location"
      ]

instance
  (MonadIO m, MonadReader env m, HasHackageApiKey env)
  => MonadHackage (ActualHackage m)
  where
  getSelf = do
    bs <- view $ hackageApiKeyL . unL
    resp <-
      httpNoBody
        $ addApiKeyAuthorization bs
        $ disableRedirects
        $ parseRequest_
          "https://hackage.haskell.org/users/account-management"
    maybe (throwIO $ GetSelfError resp) pure $ do
      a <- lookupResponseHeader hLocation resp
      b <- stripPrefix "/user/" a
      c <- stripSuffix "/manage" b
      pure $ HackageUsername c

  getMaintainedPackages (HackageUsername username) = do
    bs <- view $ hackageApiKeyL . unL
    fmap (mapMaybe packageFromMaintainers . (.groups) . getResponseBody)
      $ httpJSON @_ @HackageUser
      $ addApiKeyAuthorization bs
      $ parseRequest_
      $ "https://hackage.haskell.org/user/"
      <> unpack username

  getPackageMaintainers (Package name) = do
    bs <- view $ hackageApiKeyL . unL
    fmap (map (.username) . (.members) . getResponseBody)
      $ httpJSON @_ @HackageGroup
      $ addApiKeyAuthorization bs
      $ parseRequest_
      $ "https://hackage.haskell.org/package/"
      <> unpack name
      <> "/maintainers/"

  addPackageMaintainer (Package name) (HackageUsername user) = do
    bs <- view $ hackageApiKeyL . unL

    let
      reason :: Text
      reason = "Added by freckle/hackage-team"

    void
      $ httpNoBody
      $ addApiKeyAuthorization bs
      $ addToRequestQueryString
        [ ("user", Just $ encodeUtf8 user)
        , ("reason", Just $ encodeUtf8 reason)
        ]
      $ setRequestCheckStatus
      $ parseRequest_
      $ "POST https://hackage.haskell.org/package/"
      <> unpack name
      <> "/maintainers/"

  removePackageMaintainer (Package name) (HackageUsername user) = do
    bs <- view $ hackageApiKeyL . unL

    let
      reason :: Text
      reason = "Removed by freckle/hackage-team"

    void
      $ httpNoBody
      $ addApiKeyAuthorization bs
      $ addToRequestQueryString [("reason", Just $ encodeUtf8 reason)]
      $ setRequestCheckStatus
      $ parseRequest_
      $ "DELETE https://hackage.haskell.org/package/"
      <> unpack name
      <> "/maintainers/user/"
      <> unpack user
