module UI.CommandLine (Flag, argOrder, optGrammar, processOpts) where

import Service.WebServiceConsultation (runSearchFirstPage, runSearchPaged', runSearchSlideshow)

import MetaFeatures.Help         (runHelp)
import MetaFeatures.UnitTest     (runTest)
import MetaFeatures.LazinessDemo (runLazinessDemo)

import System.Console.GetOpt (ArgOrder (Permute), OptDescr (Option), ArgDescr (NoArg, ReqArg))


-- Vocabulary:

data Flag = Help | UnitTests | LazinessDemo | SearchService String | PagedService String | SlideService String

-- Syntax:

argOrder :: ArgOrder Flag
argOrder = Permute

optGrammar :: [OptDescr Flag]
optGrammar = [
                 Option "h?" ["help"              ] (NoArg Help)                    " ... Info about the command-line interface (flags, options)",
                 Option "t"  ["test"              ] (NoArg UnitTests)               " ... Run unit tests for the programs's main logic part: the pagination state machines (transition functions, monads, monad transformers)",
                 Option "dl" ["demo"  , "laziness"] (NoArg LazinessDemo)            " ... Run special unit tests for demonstarting the limits of laziness",
                 Option "S"  ["search", "service" ] (ReqArg SearchService "<EXPR>") " ... Consult MediaWiki API's search: 1st page of results for <EXPR> (i.e. the first ten items)",
                 Option "p"  ["pager" , "paginate"] (ReqArg PagedService  "<EXPR>") " ... Consult MediaWiki API's search: paginated results for <EXPR> (interactive pagination)",
                 Option "s"  ["slide" , "show"    ] (ReqArg SlideService  "<EXPR>") " ... Consult MediaWiki API's search: delayed slideshow forr <EXPR>"
             ]

-- Semantics:

processOpts :: ([Flag], [String], [String]) -> IO ()
processOpts ([UnitTests]         , []    ,     []   ) = runTest
processOpts ([LazinessDemo]      , []    ,     []   ) = runLazinessDemo
processOpts ([SearchService expr], []    ,     []   ) = runSearchFirstPage expr
processOpts ([PagedService expr] , []    ,     []   ) = runSearchPaged' expr
processOpts ([SlideService expr] , []    ,     []   ) = runSearchSlideshow expr
processOpts ([Help]              , []    ,     []   ) = runHelp  optGrammar
processOpts ([_]                 , _:_   ,     []   ) = putStrLn "Non-option arguments cannot be used" >> runHelp optGrammar
processOpts ([]                  , [expr],     []   ) = putStrLn ("Interpreting as abbrev for `--paginate <EXPR>") >> runSearchPaged' expr
processOpts ([]                  , []    ,     []   ) = putStrLn "Interpreting as abbrev for `--help`" >> runHelp optGrammar
processOpts ([]                  , _:_:_ ,     []   ) = putStrLn "Non-option arguments cannot be used" >> runHelp optGrammar
processOpts (_                   , _     , err@(_:_)) = mapM_ putStr err >> runHelp optGrammar
processOpts (_:_:_               , _     ,     []   ) = putStrLn "You cannot combine flags" >> runHelp optGrammar
