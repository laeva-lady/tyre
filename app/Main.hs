module Main (main) where

import Lib
import System.Environment (getArgs)

pathToPkgs :: String
pathToPkgs = "packages.txt"

handleArgs :: [String] -> [String] -> IO [String]
handleArgs pkgs (cmd : xs)
  | cmd == "sync" || cmd == "s" = do
      syncPkgsExplicit pkgs
      return pkgs
  | cmd == "add" || cmd == "a" = do
      let newPkgs = addPackageToList xs pkgs
      syncPkgsExplicit newPkgs
      return newPkgs
  | cmd == "remove" || cmd == "r" = do
      let newPkgs = removePackageFromList xs pkgs
      removePkgs xs
      return newPkgs
handleArgs pkgs _ = do
  syncPkgs
  return pkgs

main :: IO ()
main = do
  args <- getArgs
  contents <- getListOfPkgs pathToPkgs
  newPkgs <- handleArgs contents args

  writeListToFile pathToPkgs newPkgs

  return ()

