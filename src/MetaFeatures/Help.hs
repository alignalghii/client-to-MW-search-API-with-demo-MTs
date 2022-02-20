module MetaFeatures.Help (runHelp) where

import System.Console.GetOpt (OptDescr, usageInfo)
import System.Environment (getProgName)


runHelp :: [OptDescr flag] -> IO ()
runHelp optGrammar = do
    programName <- getProgName
    putStrLn ""
    putStrLn $ "A command-line client for MediaWiki's search API."
    putStrLn $ "  -  The target API is [API:Search](https://www.mediawiki.org/wiki/API:Search)."
    putStrLn $ "  -  The program's main goal is to demonstate a beginner's lightweight usage of monad transformers."
    putStrLn $ "     `StateT (Maybe PaginationToken) IO [Title]` and some more lightweight approaches to paginated API requests."
    putStrLn ""
    putStrLn $ usageInfo ("Usage: " ++ programName ++ " <OPTION>\nwhere <OPTION> can be *one* of the following options:") $ optGrammar
    putStrLn $ "Abbreviations:"
    putStrLn $ "  -  `" ++ programName ++ "`        alone is interpreted as `" ++ programName ++ " --help`"
    putStrLn $ "  -  `" ++ programName ++ " <EXPR>` is interpreted as `" ++ programName ++ " --paginate <EXPR>`"
    putStrLn ""
    putStrLn $ "Examples:"
    putStrLn $ "  -  `" ++ programName ++ " --paginate Haskell` is listing WikiMedia's Haskell-related doc titles, You can confirm or cancel pagination continuation."
    putStrLn $ "  -  `" ++ programName ++ " --search Haskell`   is listing the first ten Haskell-related doc titles."
    putStrLn $ "  -  `" ++ programName ++ " --help`             is showing this help text."
    putStrLn $ "  -  `" ++ programName ++ " --test`             is for developers and students: it runs the unit tests of the pagination state machines used by this program."
    putStrLn $ "  -  `" ++ programName ++ " --laziness`         is also for developers and students: it demonstrates the laziness behavior of lazy state monad transformers."
    putStrLn ""
