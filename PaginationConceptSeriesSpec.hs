module PaginationConceptSeriesSpec where

import Test.Hspec
import PaginationConceptSeries
import Control.Monad.State.Strict (StateT (StateT), runStateT, state, get, put)

import Aux
import SampleTypes

-- God's book has (almost) infinitely many pages, an infinite state machine is needed to fetch them:
infiniteBook :: Transition Int String
infiniteBook n = ("Page #" ++ show n, n + 1)

-- Even a finite state machine can be run (almost) forever:
clockTicking :: Transition Bool String
clockTicking True  = ("tick", False)
clockTicking False = ("tock", True )

-- Seasons are rather *paginated* here, exemplified and enumerated finitely, instead of being run forever:
exemplifySeasons :: PaginationTransition Int String
exemplifySeasons Nothing  = ("spring", Just 1 )
exemplifySeasons (Just 1) = ("summer", Just 2 )
exemplifySeasons (Just 2) = ("autumn", Just 3 )
exemplifySeasons (Just 3) = ("winter", Nothing)


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
    describe "Infinite effectless pagination" $ do
        describe "Both infinite state-jumps and finite state-jumps can have an infinite runtime" $ do
            it "God's infinite book with infinite state-jumps" $ do
                take 5 (infinite_native_pagination infiniteBook 1)    `shouldBe` ["Page #1", "Page #2", "Page #3", "Page #4", "Page #5"]
            it "A ticking clock with finite state-jumps but infinite runtime" $ do
                take 5 (infinite_native_pagination clockTicking True) `shouldBe` ["tick"   , "tock"   , "tick"   , "tock"   , "tick"   ]
    describe "Limitable effectless pagination" $ do
        describe "There are only four seasons, the sample is limited" $ do
            it "simple-function non-DRY solution" $ do
                native_pagination_nonDRY exemplifySeasons     `shouldBe` ["spring", "summer", "autumn", "winter"]
            it "simple-function but more economical (DRY) solution" $ do
                native_pagination        exemplifySeasons     `shouldBe` ["spring", "summer", "autumn", "winter"]
            it "State-monad solution" $ do
                native_pagination_SM (state exemplifySeasons) `shouldBe` ["spring", "summer", "autumn", "winter"]
    describe "Effectful pagination" $ do
        describe "Biliography server" $ do
            it "Without state transformer" $ do
                effect_pagination_noMT bibliographyTransitionEffect `shouldBe` return [["Harry Potter I", "H.P. II", "H.P. III"], ["H.P. IV", "H.P. V", "Lord of rings"], ["The Hobbit", "Maya bee", "Shrek"], ["Shrek II", "Shrek III"]]
            it "With state transformer" $ do
                effect_pagination_MT bibliographyStateT `shouldBe` return [["Harry Potter I", "H.P. II", "H.P. III"], ["H.P. IV", "H.P. V", "Lord of rings"], ["The Hobbit", "Maya bee", "Shrek"], ["Shrek II", "Shrek III"]]
