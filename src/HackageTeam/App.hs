module HackageTeam.App
  ( App (..)
  , loadApp
  , AppT
  , runAppT
  ) where

import HackageTeam.Prelude

import Data.ByteString.Char8 qualified as BS8
import HackageTeam.HackageApi
import HackageTeam.HackageApi.Actual
import HackageTeam.Options
import System.Environment (getEnv)

data App = App
  { options :: Options
  , hackageApiKey :: HackageApiKey
  }

instance HasOptions App where
  optionsL = lens (.options) $ \x y -> x {options = y}

instance HasHackageApiKey App where
  hackageApiKeyL = lens (.hackageApiKey) $ \x y -> x {hackageApiKey = y}

loadApp :: Options -> IO App
loadApp options = App options <$> (toKey <$> getEnv "HACKAGE_API_KEY")
 where
  toKey = HackageApiKey . BS8.pack

newtype AppT m a = AppT
  { unwrap :: ReaderT App (LoggingT m) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadLogger
    , MonadReader App
    )
  deriving (MonadHackage) via ActualHackage (AppT m)

runAppT :: MonadIO m => App -> AppT m a -> m a
runAppT app =
  runStdoutLoggingT
    . (if app.options.verbose then id else filterDebug)
    . flip runReaderT app
    . (.unwrap)

filterDebug :: LoggingT m a -> LoggingT m a
filterDebug = filterLogger $ const (>= LevelInfo)
