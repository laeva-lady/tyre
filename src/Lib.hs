module Lib
  ( getListOfPkgs,
    syncPkgsExplicit,
    syncPkgs,
    addPackageToList,
    removePackageFromList,
    removePkgs,
    writeListToFile,
  )
where

import Control.Exception (evaluate)
import Data.Char (isSpace)
import Data.List (sort)
import System.Process (callProcess)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

getListOfPkgs :: String -> IO [String]
getListOfPkgs filepath = do
  contents <- readFile filepath
  _ <- evaluate (length contents)
  return $ map trim $ lines contents

addPackageToList :: [String] -> [String] -> [String]
addPackageToList names pkgs = sort $ names ++ pkgs

removePackageFromList :: [String] -> [String] -> [String]
removePackageFromList names pkgs = sort $ filter (`notElem` names) pkgs

syncPkgsExplicit :: [String] -> IO ()
syncPkgsExplicit [] = putStrLn "No packages to sync"
syncPkgsExplicit pkgs = do callProcess "paru" ("-Syu" : "--needed" : pkgs)

syncPkgs :: IO ()
syncPkgs = do callProcess "paru" ["-Syu"]

removePkgs :: [String] -> IO ()
removePkgs names = do callProcess "paru" ("-Rs" : names)

writeListToFile :: String -> [String] -> IO ()
writeListToFile filepath pkgs = do
  writeFile filepath $ unlines $ map trim pkgs
