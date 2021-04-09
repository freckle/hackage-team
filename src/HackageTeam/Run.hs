module HackageTeam.Run
  ( run
  ) where

import HackageTeam.Prelude

import HackageTeam.HackageApi
import HackageTeam.Options

run
  :: (MonadLogger m, MonadHackage m, MonadReader env m, HasOptions env)
  => [HackageUsername]
  -> m ()
run expectedMaintainers = do
  Options {..} <- view optionsL

  username <- getSelf
  logDebug $ "Running as Maintainer: " <> display username

  packages <- filter (`notElem` oExclude) <$> getMaintainedPackages username
  traverse_ (logDebug . ("Packages: " <>) . display) packages

  for_ packages $ \package -> do
    logInfo $ "Checking package: " <> display package
    maintainers <- filter (/= username) <$> getPackageMaintainers package
    traverse_ (logDebug . ("Maintainers: " <>) . display) maintainers

    unless oSuppressAdds
      $ for_ (filter (`notElem` maintainers) expectedMaintainers)
      $ \maintainer -> do
          logInfo $ "Expected, not present: " <> display maintainer
          when oFix $ addPackageMaintainer package maintainer

    unless oSuppressRemoves
      $ for_ (filter (`notElem` expectedMaintainers) maintainers)
      $ \maintainer -> do
          logInfo $ "Present, not expected: " <> display maintainer
          when oFix $ removePackageMaintainer package maintainer
