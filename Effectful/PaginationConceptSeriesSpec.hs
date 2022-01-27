module Effectful.PaginationConceptSeriesSpec where

import Test.Hspec
import Effectful.PaginationConceptSeries

import Aux
import SampleTypes
import Data.ErrorEffect (ErrorEffect, errorfree, withError)
import Control.Monad.State.Strict (StateT (StateT))


-- An effectful state transition function for sample.
-- We build an effect (technically: a *monad*) on top of a pure transition function.
-- Most practical would be building an IO-effect upon it: a server that can be requested only thorough HTTP connection (`IO` monad).
-- But instead of the hard-to-test `IO` monad, we use the simpler `ErrorEffect` monad here for testing.

type BookTitle = String

bibliographyTransitionEffect :: PaginationEffect ContinuationToken ErrorEffect [BookTitle]
bibliographyTransitionEffect Nothing               = errorfree (["Harry Potter I", "H.P. II" , "H.P. III"     ], Just "8-books-more")
bibliographyTransitionEffect (Just "8-books-more") = errorfree (["H.P. IV"       , "H.P. V"  , "Lord of rings"], Just "5-books-more")
bibliographyTransitionEffect (Just "5-books-more") = errorfree (["The Hobbit"    , "Maya bee", "Shrek"        ], Just "2-books-more")
bibliographyTransitionEffect (Just "2-books-more") = errorfree (["Shrek II"      , "Shrek III"                ], Nothing)
bibliographyTransitionEffect (Just tkn           ) = withError $ "<<UNRECOGNIZED TOKEN>>[" ++ show tkn ++ "]"

-- Now here are the tests (specifications) based on the examples above:

spec :: Spec
spec = do
    describe "Effectful pagination" $ do
        describe "Biliography server" $ do
            it "Without state transformer" $ do
                pagination_noMT bibliographyTransitionEffect `shouldBe` errorfree [["Harry Potter I", "H.P. II", "H.P. III"], ["H.P. IV", "H.P. V", "Lord of rings"], ["The Hobbit", "Maya bee", "Shrek"], ["Shrek II", "Shrek III"]]
            it "With state transformer" $ do
                pagination_MT (StateT bibliographyTransitionEffect) `shouldBe` errorfree [["Harry Potter I", "H.P. II", "H.P. III"], ["H.P. IV", "H.P. V", "Lord of rings"], ["The Hobbit", "Maya bee", "Shrek"], ["Shrek II", "Shrek III"]]