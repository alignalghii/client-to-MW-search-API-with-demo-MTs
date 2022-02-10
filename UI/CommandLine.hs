module UI.CommandLine (Flag, argOrder, optGrammar, processOpts) where

import Service.WebServiceConsultation (runSearchFirstPage, runSearchPaged)

import MetaFeatures.Help         (runHelp)
import MetaFeatures.UnitTest        (runUnitTests)
import MetaFeatures.IntegrationTest (runIntegrationTests)
import MetaFeatures.LazinessDemo (runLazinessDemo)

import System.Console.GetOpt (ArgOrder (Permute), OptDescr (Option), ArgDescr (NoArg, ReqArg))


-- Vocabulary:

data Flag = Help | UnitTests | IntegrationTests | LazinessDemo | SearchService String | PagedService String

-- Syntax:

argOrder :: ArgOrder Flag
argOrder = Permute

optGrammar :: [OptDescr Flag]
optGrammar = [
                 Option "h?" ["help"              ] (NoArg Help)                    " ... Info about the command-line interface (flags, options)",
                 Option "tu" ["test" , "unit-test"] (NoArg UnitTests)               " ... Run unit tests for the programs's main logic part: the pagination state machines (transition functions, monads, monad transformers)",
                 Option "i"  ["itest", "intgr-test"] (NoArg IntegrationTests)       " ... Run integrations tests (e.g. service exception handling)",
                 Option "dl" ["demo"  , "laziness"] (NoArg LazinessDemo)            " ... Run special unit tests for demonstarting the limits of laziness",
                 Option "S"  ["search", "service" ] (ReqArg SearchService "<EXPR>") " ... Consult MediaWiki API's search: 1st page of results for <EXPR> (i.e. the first ten items)",
                 Option "p"  ["pager" , "paginate"] (ReqArg PagedService  "<EXPR>") " ... Consult MediaWiki API's search: paginated results for <EXPR> (interactive pagination)"
             ]

-- Semantics:

processOpts :: ([Flag], [String], [String]) -> IO ()
processOpts ([UnitTests]         , []    ,     []   ) = runUnitTests
processOpts ([IntegrationTests]  , []    ,     []   ) = runIntegrationTests
processOpts ([LazinessDemo]      , []    ,     []   ) = runLazinessDemo
processOpts ([SearchService expr], []    ,     []   ) = runSearchFirstPage expr
processOpts ([PagedService expr] , []    ,     []   ) = runSearchPaged expr
processOpts ([Help]              , []    ,     []   ) = runHelp  optGrammar
processOpts ([_]                 , _:_   ,     []   ) = putStrLn "Non-option arguments cannot be used" >> runHelp optGrammar
processOpts ([]                  , [expr],     []   ) = putStrLn ("Interpreting as abbrev for `--paginate <EXPR>") >> runSearchPaged expr
processOpts ([]                  , []    ,     []   ) = putStrLn "Interpreting as abbrev for `--help`" >> runHelp optGrammar
processOpts ([]                  , _:_:_ ,     []   ) = putStrLn "Non-option arguments cannot be used" >> runHelp optGrammar
processOpts (_                   , _     , err@(_:_)) = mapM_ putStr err >> runHelp optGrammar
processOpts (_:_:_               , _     ,     []   ) = putStrLn "You cannot combine flags" >> runHelp optGrammar
