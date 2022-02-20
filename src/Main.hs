module Main (main) where

import UI.CommandLine (argOrder, optGrammar, processOpts)

import System.Console.GetOpt (getOpt)
import System.Environment (getArgs)


main :: IO ()
main = getArgs >>= processArgs

processArgs :: [String] -> IO ()
processArgs = processOpts . getOpt argOrder optGrammar

