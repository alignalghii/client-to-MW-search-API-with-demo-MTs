module Effectful.PaginationConceptSeriesSpec where

import Test.Hspec
import Effectful.PaginationConceptSeries
import Control.Monad.State.Strict (StateT (StateT), runStateT, state, get, put)

import Aux
import SampleTypes


-- Let us simulate an IO effect: a server that can be requested only thorough HTTP connection (IO monad):

type BookTitle = String

bibliographyTransition :: PaginationTransition ContinuationToken [BookTitle]
bibliographyTransition Nothing                              = (["Harry Potter I", "H.P. II" , "H.P. III"     ], Just "token-for-remaining-8-books")
bibliographyTransition (Just "token-for-remaining-8-books") = (["H.P. IV"       , "H.P. V"  , "Lord of rings"], Just "token-for-remaining-5-books")
bibliographyTransition (Just "token-for-remaining-5-books") = (["The Hobbit"    , "Maya bee", "Shrek"        ], Just "token-for-remaining-2-books")
bibliographyTransition (Just "token-for-remaining-2-books") = (["Shrek II"      , "Shrek III"                ], Nothing                           )
bibliographyTransition (Just tkn                          ) = (["<<UNRECOGNIZED TOKEN>>[" ++ show tkn ++ "]" ], Nothing                           )


bibliographyTransitionEffect :: PaginationEffect ContinuationToken ErrorM [BookTitle]
bibliographyTransitionEffect = return . bibliographyTransition

bibliographyStateT :: PaginationStateT ContinuationToken ErrorM [BookTitle]
bibliographyStateT = pureTransitionFunctionToStateT bibliographyTransition


-- Now here are the tests (specifications) based on the examples above:

spec :: Spec
spec = do
    describe "Effectful pagination" $ do
        describe "Biliography server" $ do
            it "Without state transformer" $ do
                effect_pagination_noMT bibliographyTransitionEffect `shouldBe` return [["Harry Potter I", "H.P. II", "H.P. III"], ["H.P. IV", "H.P. V", "Lord of rings"], ["The Hobbit", "Maya bee", "Shrek"], ["Shrek II", "Shrek III"]]
            it "With state transformer" $ do
                effect_pagination_MT bibliographyStateT `shouldBe` return [["Harry Potter I", "H.P. II", "H.P. III"], ["H.P. IV", "H.P. V", "Lord of rings"], ["The Hobbit", "Maya bee", "Shrek"], ["Shrek II", "Shrek III"]]
