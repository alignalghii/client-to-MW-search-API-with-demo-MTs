module Effectful.PaginationConceptSeriesSpec where

import Test.Hspec
import Effectful.PaginationConceptSeries

import Aux
import SampleTypes
import Data.ErrorEffect (errorfree)


-- An effectless state transition function for sample:

type BookTitle = String

bibliographyTransition :: PaginationTransition ContinuationToken [BookTitle]
bibliographyTransition Nothing                              = (["Harry Potter I", "H.P. II" , "H.P. III"     ], Just "token-for-remaining-8-books")
bibliographyTransition (Just "token-for-remaining-8-books") = (["H.P. IV"       , "H.P. V"  , "Lord of rings"], Just "token-for-remaining-5-books")
bibliographyTransition (Just "token-for-remaining-5-books") = (["The Hobbit"    , "Maya bee", "Shrek"        ], Just "token-for-remaining-2-books")
bibliographyTransition (Just "token-for-remaining-2-books") = (["Shrek II"      , "Shrek III"                ], Nothing                           )
bibliographyTransition (Just tkn                          ) = (["<<UNRECOGNIZED TOKEN>>[" ++ show tkn ++ "]" ], Nothing                           )

-- Let us simulate an IO effect: a server that can be requested only thorough HTTP connection (IO monad)
-- But instead of the hard-to-test IO monad, we use the ErrorEffet monad for testing.
-- We build ErrorEffect monad on top of the above pure transition function.
-- Now here are the tests (specifications) based on the examples above:

spec :: Spec
spec = do
    describe "Effectful pagination" $ do
        describe "Biliography server" $ do
            it "Without state transformer" $ do
                pagination_noMT (return . bibliographyTransition) `shouldBe` errorfree [["Harry Potter I", "H.P. II", "H.P. III"], ["H.P. IV", "H.P. V", "Lord of rings"], ["The Hobbit", "Maya bee", "Shrek"], ["Shrek II", "Shrek III"]]
            it "With state transformer" $ do
                pagination_MT (pureTransitionFunctionToStateT bibliographyTransition) `shouldBe` errorfree [["Harry Potter I", "H.P. II", "H.P. III"], ["H.P. IV", "H.P. V", "Lord of rings"], ["The Hobbit", "Maya bee", "Shrek"], ["Shrek II", "Shrek III"]]
