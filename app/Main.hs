module Main
  ( main
  ) where

import HackageTeam.Prelude

import HackageTeam.App
import HackageTeam.Options
import HackageTeam.Run

main :: IO ()
main = do
  options <- parseOptions
  maintainers <- getMaintainers
  app <- loadApp options
  checkFailed <- runAppT app $ execStateT (run options maintainers) False
  when (checkFailed && not options.fix && not options.exitZero) exitFailure
