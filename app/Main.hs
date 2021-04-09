module Main
  ( main
  ) where

import HackageTeam.Prelude

import HackageTeam.App
import HackageTeam.HackageApi (HackageUsername(..))
import HackageTeam.Options
import HackageTeam.Run

main :: IO ()
main = do
  options <- parseOptions
  app <- loadApp options
  runAppT app $ run expectedMaintainers

-- TODO: read stdin
expectedMaintainers :: [HackageUsername]
expectedMaintainers =
  [ HackageUsername "PatrickBrisbin"
  , HackageUsername "cbeav"
  , HackageUsername "cdparks"
  , HackageUsername "dukerutledge"
  , HackageUsername "halogenandtoast"
  , HackageUsername "mjgpy3"
  ]
