module MetaInfo.UnitTest where

import qualified PaginationStateMachines.Effectless.PaginationConceptSeriesSpec as EffectlessSpecs (spec)
import qualified PaginationStateMachines.Effectful.PaginationConceptSeriesSpec  as EffectfulSpecs  (spec)

import Test.Hspec (hspec)
import System.Environment (withArgs)


runTest :: IO ()
runTest = withArgs [] $ hspec $ EffectlessSpecs.spec >> EffectfulSpecs.spec
