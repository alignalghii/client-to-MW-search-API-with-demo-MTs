module MetaInfo.Help (runHelp) where

import System.Console.GetOpt (OptDescr, usageInfo)
import System.Environment (getProgName)


runHelp :: [OptDescr flag] -> IO ()
runHelp optGrammar = do
    programName <- getProgName
    putStrLn $ usageInfo ("Usage: " ++ programName ++ " <OPTION>\nwhere <OPTION> can be *one* of the following options:") $ optGrammar
    putStrLn $ "Abbreviations:\n    `" ++ programName ++ "` alone is interpreted as `" ++ programName ++ " --help`"
    putStrLn $ "    `" ++ programName ++ " <EXPR>` is interpreted as `" ++ programName ++ " --paginate <EXPR>`"
