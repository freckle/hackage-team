module HackageTeam.HackageApi
  (
  -- * Settings
    HackageApiKey(..)
  , HasHackageApiKey(..)

  -- * API Types
  , Package(..)
  , Maintainer(..)

  -- * Effects
  , MonadHackage(..)

  -- ** Implementations
  --, ActualHackage
  ) where

import HackageTeam.Prelude

newtype HackageApiKey = HackageApiKey
  { unHackageApiKey :: ByteString
  }

class HasHackageApiKey env where
  hackageApiKeyL :: Lens' env HackageApiKey

instance HasHackageApiKey HackageApiKey where
  hackageApiKeyL = id

newtype Package = Package Text

newtype Maintainer = Maintainer Text

class Monad m => MonadHackage m where
  getSelf :: m Maintainer

  getMyPackages :: m [Package]

  getPackageMaintainers :: Package -> m [Maintainer]

  addPackageMaintainer :: Package -> Maintainer -> m ()

  removePackageMaintainer :: Package -> Maintainer -> m ()
