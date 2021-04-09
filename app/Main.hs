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
  app <- loadApp options
  runAppT app run
