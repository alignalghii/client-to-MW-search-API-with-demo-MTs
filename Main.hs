module Main (main) where

import WebServiceConsultation (runSearch)

import Test.Hspec (hspec)
import qualified Effectless.PaginationConceptSeriesSpec as EffectlessSpecs (spec)
import qualified Effectful.PaginationConceptSeriesSpec  as EffectfulSpecs  (spec)

import System.Environment (getArgs)
import Data.Maybe (listToMaybe)


main, runTests :: IO ()
main = fmap listToMaybe getArgs >>= maybe runTests runSearch
runTests = do
    putStrLn "No searchphrase provided, so I am running unit tests instead"
    hspec $ EffectlessSpecs.spec >> EffectfulSpecs.spec
