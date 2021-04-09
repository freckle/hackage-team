module HackageTeam.Options
  ( Options(..)
  , HasOptions(..)
  , parseOptions
  ) where

import HackageTeam.Prelude

import HackageTeam.HackageApi
import Options.Applicative

data Options = Options
  { oSuppressAdds :: Bool
  , oSuppressRemoves :: Bool
  , oExclude :: [Package]
  , oFix :: Bool
  , oVerbose :: Bool
  }

class HasOptions env where
  optionsL :: Lens' env Options

instance HasOptions Options where
  optionsL = id

parseOptions :: IO Options
parseOptions = execParser $ withInfo "Maintain Hackage teams" options

withInfo :: Text -> Parser a -> ParserInfo a
withInfo d p = info (p <**> helper) $ progDesc (unpack d) <> fullDesc

-- brittany-disable-next-binding

options :: Parser Options
options =
  Options
    <$> switch
      (  long "no-add"
      <> help "Don't process maintainers that need adding"
      )
    <*> switch
      (  long "no-remove"
      <> help "Don't process maintainers that need removing"
      )
    <*> many (option (eitherReader readPackage)
      (  long "exclude"
      <> metavar "PACKAGE"
      <> help "Don't process PACKAGE"
      ))
    <*> switch
      (  long "fix"
      <> help "Actually add/remove as required"
      )
    <*> switch
      (  long "verbose"
      <> help "Log verbosely"
      )
