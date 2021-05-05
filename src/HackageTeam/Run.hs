module HackageTeam.Run
  ( run
  , getMaintainers
  ) where

import HackageTeam.Prelude

import HackageTeam.HackageApi
import HackageTeam.Options
import qualified RIO.ByteString as BS
import qualified RIO.Text as T

run
  :: (MonadState Bool m, MonadLogger m, MonadHackage m)
  => Options
  -> [HackageUsername]
  -> m ()
run Options {..} expectedMaintainers = do
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
          put True
          logWarn $ "Expected, not present: " <> display maintainer
          when oFix $ addPackageMaintainer package maintainer

    unless oSuppressRemoves
      $ for_ (filter (`notElem` expectedMaintainers) maintainers)
      $ \maintainer -> do
          put True
          logWarn $ "Present, not expected: " <> display maintainer
          when oFix $ removePackageMaintainer package maintainer

-- | Read a list of expected maintainers, one per line, on @stdin@
--
-- This is only extracted so 'run' can be tested in isolation.
--
getMaintainers :: MonadIO m => m [HackageUsername]
getMaintainers = readMaintainers <$> BS.getContents

readMaintainers :: ByteString -> [HackageUsername]
readMaintainers = map HackageUsername . T.lines . decodeUtf8
