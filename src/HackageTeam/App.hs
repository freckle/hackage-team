module HackageTeam.App
  ( App(..)
  , loadApp
  , AppT
  , runAppT
  ) where

import HackageTeam.Prelude

import HackageTeam.HackageApi
import HackageTeam.Options

data App = App
  { appOptions :: Options
  , appHackageApiKey :: HackageApiKey
  }

instance HasOptions App where
  optionsL = lens appOptions $ \x y -> x { appOptions = y }

instance HasHackageApiKey App where
  hackageApiKeyL = lens appHackageApiKey $ \x y -> x { appHackageApiKey = y }

loadApp :: Options -> IO App
loadApp = undefined

newtype AppT m a = AppT
  { unAppT :: ReaderT App (LoggingT m) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader App
    )

runAppT :: MonadIO m => App -> AppT m a -> m a
runAppT app = runStdoutLoggingT . flip runReaderT app . unAppT
