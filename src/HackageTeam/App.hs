module HackageTeam.App
  ( App(..)
  , loadApp
  , AppT
  , runAppT
  ) where

import HackageTeam.Prelude

import qualified Data.ByteString.Char8 as BS8
import HackageTeam.HackageApi
import HackageTeam.Options
import System.Environment (getEnv)

data App = App
  { appOptions :: Options
  , appHackageApiKey :: HackageApiKey
  }

instance HasOptions App where
  optionsL = lens appOptions $ \x y -> x { appOptions = y }

instance HasHackageApiKey App where
  hackageApiKeyL = lens appHackageApiKey $ \x y -> x { appHackageApiKey = y }

loadApp :: Options -> IO App
loadApp options = App options <$> (toKey <$> getEnv "HACKAGE_API_KEY")
  where toKey = HackageApiKey . BS8.pack

newtype AppT m a = AppT
  { unAppT :: ReaderT App (LoggingT m) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadLogger
    , MonadReader App
    )
  deriving MonadHackage via ActualHackage (AppT m)

runAppT :: MonadIO m => App -> AppT m a -> m a
runAppT app =
  runStdoutLoggingT
    . (if oVerbose $ appOptions app then id else filterDebug)
    . flip runReaderT app
    . unAppT

filterDebug :: LoggingT m a -> LoggingT m a
filterDebug = filterLogger $ const (>= LevelInfo)
