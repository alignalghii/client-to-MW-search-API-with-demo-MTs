module MetaFeatures.UnitTest (runUnitTests) where

import qualified PaginationStateMachines.Effectless.PaginationConceptSeriesSpec as EffectlessSpecs (spec)
import qualified PaginationStateMachines.Effectful.PaginationConceptSeriesSpec  as EffectfulSpecs  (spec)

import Test.Hspec (hspec)
import System.Environment (withArgs)


runUnitTests :: IO ()
runUnitTests = withArgs [] $ hspec $ EffectlessSpecs.spec >> EffectfulSpecs.spec
