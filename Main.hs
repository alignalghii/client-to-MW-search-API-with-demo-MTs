module Main (main) where

import System.Console.GetOpt
import Service.WebServiceConsultation (runSearchFirstPage)

import Test.Hspec (hspec)
import qualified Effectless.PaginationConceptSeriesSpec as EffectlessSpecs (spec)
import qualified Effectful.PaginationConceptSeriesSpec  as EffectfulSpecs  (spec)

import System.Environment (getProgName, getArgs, withArgs)


main :: IO ()
main = getArgs >>= processArgs

processArgs :: [String] -> IO ()
processArgs = processOpts . getOpt Permute optGrammar

data Flag = SearchService String | Test | Help

optGrammar :: [OptDescr Flag]
optGrammar = [
                 Option "S"  ["search", "service"] (ReqArg SearchService "<EXPR>") " # Consult with Wikipedia's search service: search for <EXPR>",
                 Option "t"  ["test"             ] (NoArg Test)                    " # Run unit tests",
                 Option "h?" ["help"             ] (NoArg Help)                    " # Info about the command-line interface (flags, options)"
             ]

processOpts :: ([Flag], [String], [String]) -> IO ()
processOpts ([Test]              , []    ,     []   ) = runTest
processOpts ([SearchService expr], []    ,     []   ) = runSearchFirstPage expr
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
