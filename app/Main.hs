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
  runAppT app $ run options maintainers
