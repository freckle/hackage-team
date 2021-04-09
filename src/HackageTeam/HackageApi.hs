{-# LANGUAGE UndecidableInstances #-}

module HackageTeam.HackageApi
  (
  -- * Settings
    HackageApiKey(..)
  , HasHackageApiKey(..)

  -- * API Types
  , HackageUser(..)
  , HackageUsername(..)
  , Package(..)
  , readPackage

  -- * Effects
  , MonadHackage(..)

  -- ** Implementations
  , ActualHackage(..)
  ) where

import HackageTeam.Prelude

import Data.Aeson
import HackageTeam.HTTP
import Network.HTTP.Types.Header (hLocation)
import RIO.Text (stripPrefix, stripSuffix)

newtype HackageApiKey = HackageApiKey
  { unHackageApiKey :: ByteString
  }

unL :: Lens' HackageApiKey ByteString
unL = lens unHackageApiKey $ \x y -> x { unHackageApiKey = y }

class HasHackageApiKey env where
  hackageApiKeyL :: Lens' env HackageApiKey

instance HasHackageApiKey HackageApiKey where
  hackageApiKeyL = id

newtype HackageUser = HackageUser
  { groups :: [Text]
  }
  deriving stock Generic
  deriving anyclass FromJSON

newtype HackageUsername = HackageUsername Text
  deriving stock Eq
  deriving newtype (Display, FromJSON)

newtype Package = Package Text
  deriving stock Eq
  deriving newtype Display

readPackage :: String -> Either String Package
readPackage = Right . Package . pack

packageFromGroup :: Text -> Maybe Package
packageFromGroup a = do
  b <- stripPrefix "/package/" a
  c <- stripSuffix "/maintainers" b
  pure $ Package c

newtype HackageGroup = HackageGroup
  { members :: [HackageGroupMember]
  }
  deriving stock Generic
  deriving anyclass FromJSON

newtype HackageGroupMember = HackageGroupMember
  { username :: HackageUsername
  }
  deriving stock Generic
  deriving anyclass FromJSON

class Monad m => MonadHackage m where
  getSelf :: m HackageUsername

  getMaintainedPackages :: HackageUsername -> m [Package]

  getPackageMaintainers :: Package -> m [HackageUsername]

  addPackageMaintainer :: Package -> HackageUsername -> m ()

  removePackageMaintainer :: Package -> HackageUsername -> m ()

newtype ActualHackage m a = ActualHackage
  { unActualHackage :: m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad , MonadIO
    , MonadLogger
    , MonadReader env
    )

instance (MonadIO m, MonadReader env m, HasHackageApiKey env)
  => MonadHackage (ActualHackage m) where
  getSelf = do
    bs <- view $ hackageApiKeyL . unL

    -- We need to disable automatic redirect following and do it ourselves,
    -- otherwise the JSON Accept header doesn't seem to make it to the second
    -- response and we get HTML
    redirect <-
      httpLbs $ addApiKeyAuthorization bs $ disableRedirects $ parseRequest_
        "https://hackage.haskell.org/users/account-management"

    let
      mUsername = do
        a <- lookupResponseHeader hLocation redirect
        b <- stripPrefix "/user/" a
        c <- stripSuffix "/manage" b
        pure $ HackageUsername c

    maybe (throwString "TODO") pure mUsername

  getMaintainedPackages (HackageUsername username) = do
    bs <- view $ hackageApiKeyL . unL
    resp <-
      httpJSON
      $ addApiKeyAuthorization bs
      $ parseRequest_
      $ "https://hackage.haskell.org/user/"
      <> unpack username
    pure $ mapMaybe packageFromGroup $ groups $ getResponseBody resp

  getPackageMaintainers (Package name) = do
    bs <- view $ hackageApiKeyL . unL
    resp <-
      httpJSON
      $ addApiKeyAuthorization bs
      $ parseRequest_
      $ "https://hackage.haskell.org/package/"
      <> unpack name
      <> "/maintainers/"
    pure $ map username $ members $ getResponseBody resp

  addPackageMaintainer = undefined

  removePackageMaintainer = undefined
