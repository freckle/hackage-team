module HackageTeam.Options
  ( Options(..)
  , HasOptions(..)
  , parseOptions
  ) where

import HackageTeam.Prelude

newtype Options = Options
  { oVerbose :: Bool
  }

class HasOptions env where
  optionsL :: Lens' env Options

instance HasOptions Options where
  optionsL = id

parseOptions :: IO Options
parseOptions = undefined
