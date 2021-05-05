module Main
  ( main
  ) where

import HackageTeam.Prelude

import HackageTeam.App
import HackageTeam.Options
import HackageTeam.Run

main :: IO ()
main = do
  options@Options {..} <- parseOptions
  maintainers <- getMaintainers
  app <- loadApp options
  checkFailed <- runAppT app $ execStateT (run options maintainers) False
  when (checkFailed && not oFix && not oExitZero) exitFailure
