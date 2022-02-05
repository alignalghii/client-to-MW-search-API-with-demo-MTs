module Main (main) where

import System.Console.GetOpt
import Service.WebServiceConsultation (runSearchFirstPage, runSearchPaged, runSearchPaged', runSearchInteract)

import Test.Hspec (hspec)
import qualified Effectless.PaginationConceptSeriesSpec as EffectlessSpecs (spec)
import qualified Effectful.PaginationConceptSeriesSpec  as EffectfulSpecs  (spec)

import System.Environment (getProgName, getArgs, withArgs)


main :: IO ()
main = getArgs >>= processArgs

processArgs :: [String] -> IO ()
processArgs = processOpts . getOpt Permute optGrammar

data Flag = Help | Test | SearchService String | PagedService String | InteractService String

optGrammar :: [OptDescr Flag]
optGrammar = [
                 Option "t"  ["test"             ] (NoArg Test)                    " # Run unit tests",
                 Option "h?" ["help"             ] (NoArg Help)                    " # Info about the command-line interface (flags, options)",
                 Option "S"  ["search", "service"] (ReqArg SearchService   "<EXPR>") " # Consult MediaWiki API's search: 1st page of results for <EXPR>",
                 Option "p"  ["pager", "paginate"] (ReqArg PagedService    "<EXPR>") " # Consult MediaWiki API's search: paginated results for <EXPR>",
                 Option "i"  ["interactive"      ] (ReqArg InteractService "<EXPR>") " # Consult MediaWiki API's search: interactive paging for <EXPR>"
             ]

processOpts :: ([Flag], [String], [String]) -> IO ()
processOpts ([Test]                , []    ,     [] ) = runTest
processOpts ([SearchService expr]  , []    ,     [] ) = runSearchFirstPage expr
processOpts ([PagedService expr]   , []    ,     [] ) = runSearchPaged' expr
processOpts ([InteractService expr], []    ,     [] ) = runSearchInteract expr
processOpts ([Help]              , []    ,     []   ) = runHelp
processOpts ([_]                 , _:_   ,     []   ) = putStrLn "Non-option arguments cannot be used" >> runHelp
processOpts ([]                  , [expr],     []   ) = putStrLn ("Interpreting as abbrev for `--search <EXPR>") >> runSearchFirstPage expr
processOpts ([]                  , []    ,     []   ) = putStrLn "Interpreting as abbrev for `--help`" >> runHelp
processOpts ([]                  , _:_:_ ,     []   ) = putStrLn "Non-option arguments cannot be used" >> runHelp
processOpts (_                   , _     , err@(_:_)) = mapM_ putStr err >> runHelp
processOpts (_:_:_               , _     ,     []   ) = putStrLn "You cannot combine flags" >> runHelp

runTest, runHelp :: IO ()

runTest = withArgs [] $ hspec $ EffectlessSpecs.spec >> EffectfulSpecs.spec


runHelp = do
    programName <- getProgName
    putStrLn $ usageInfo ("Usage: " ++ programName ++ " <OPTION>\nwhere <OPTION> can be *one* of the following options:") optGrammar
    putStrLn $ "Abbreviations:\n    `" ++ programName ++ "` alone is interpreted as `" ++ programName ++ " --help`"
    putStrLn $ "    `" ++ programName ++ " <EXPR>` is interpreted as `" ++ programName ++ " --search <EXPR>`"
