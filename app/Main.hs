--
-- EPITECH PROJECT, 2021
-- B-FUN-400-TLS-4-1-compressor-justine.fricou
-- File description:
-- Main
--

module Main where

import System.Console.GetOpt
import Control.Monad
import System.IO
import System.Exit
import System.Environment

import Option (getConfig, Options(..), showHelp, parseNbColors, parseLimit, checkOptions, checkFilePath)
import Parsing (parsePixels)
import Display (showClustersList)
import DataTypes (Cluster(..), Color(..), Coordinates(..), Pixel(..))
import KMeans (getNRandomColors, getOutput)
import Control.Exception

main :: IO ()
main = do
   args <- getArgs
   config <- getConfig args
   checkFilePath "" (filepath config)
   handle <- openFile (filepath config) ReadMode
   checkOptions config
   content <- hGetContents handle
   randColors <- getNRandomColors $ nbColors config
   putStr $ getOutput randColors (parsePixels $ lines content) (limit config)
   hClose handle