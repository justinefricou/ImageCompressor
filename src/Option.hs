--
-- EPITECH PROJECT, 2021
-- B-FUN-400-TLS-4-1-compressor-justine.fricou
-- File description:
-- Option
--

module Option (getConfig, Options(..), showHelp,
parseNbColors, parseLimit, checkOptions, checkFilePath) where
import Text.Read

import System.Console.GetOpt
import Control.Monad
import System.IO
import System.Exit
import System.Environment
import Control.Exception

data Options = Options {
   nbColors :: Int,
   limit :: Float,
   filepath :: String
} deriving (Show)

getConfig :: [String] -> IO Options
getConfig args = do
   let (actions, nonOptions, errors) = getOpt RequireOrder options args
   opts <- foldl (>>=) (return startOptions) actions
   let Options { nbColors = colors
               , limit = limit
               , filepath = fpath   } = opts
   return opts

startOptions :: Options
startOptions = Options {
   nbColors = 0,
   limit = -1,
   filepath = ""
}

checkOptions :: Options -> IO ()
checkOptions opts | nbColors opts == -1 = exitWith $ ExitFailure 84
                  | nbColors opts < 1 =  exitWith $ ExitFailure 84
                  | limit opts < 0.0 =  exitWith $ ExitFailure 84
                  | limit opts == -1.0 = exitWith $ ExitFailure 84
                  | filepath opts == "" = exitWith $ ExitFailure 84
                  | otherwise = putStr ""

checkFilePath :: String -> FilePath -> IO String
checkFilePath def filePath = readFile filePath `catch`
    \e -> const (return def) (e :: IOException) >> exitWith $ ExitFailure 84

parseNbColors :: String -> Int
parseNbColors arg = case readEither arg of
  Right arg -> read arg
  Left arg -> -1

parseLimit :: String -> Float
parseLimit arg = case readEither arg of
  Left arg -> -1.0
  Right arg -> read arg

showHelp :: String
showHelp = "USAGE: ./imageCompressor -n N -l L -f F\n\nN\tnumber of\
    \colors in the final image\nL\tconvergence limit\nF\tpath\
    \to the file containing the colors of the pixels"

options :: [OptDescr (Options -> IO Options)]
options = [
    Option "f" ["fpath"] (ReqArg (\arg opt -> return opt {filepath = arg})
    "FILE") "Input file",
    Option "l" ["limit"] (ReqArg (\arg opt -> return opt {limit = read arg})
    "float") "convergence Limit",
    Option "n" ["colors"] (ReqArg
    (\arg opt -> return opt {nbColors = read arg}) "int") "color final image",
    Option "h" ["help"] (NoArg (\_ ->
    (putStrLn showHelp >> exitWith ExitSuccess))) "Show help"
   ]
