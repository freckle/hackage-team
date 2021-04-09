module HackageTeam.HackageApi
  ( HackageUsername(..)
  , Package(..)
  , MonadHackage(..)
  ) where

import HackageTeam.Prelude

import Data.Aeson

newtype HackageUsername = HackageUsername Text
  deriving stock Eq
  deriving newtype (Display, FromJSON)

newtype Package = Package Text
  deriving stock Eq
  deriving newtype Display

class Monad m => MonadHackage m where
  getSelf :: m HackageUsername

  getMaintainedPackages :: HackageUsername -> m [Package]

  getPackageMaintainers :: Package -> m [HackageUsername]

  addPackageMaintainer :: Package -> HackageUsername -> m ()

  removePackageMaintainer :: Package -> HackageUsername -> m ()
