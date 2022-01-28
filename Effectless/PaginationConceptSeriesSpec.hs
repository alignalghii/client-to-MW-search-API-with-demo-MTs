module Effectless.PaginationConceptSeriesSpec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Effectless.PaginationConceptSeries (infinite_pagination, pagination_nonDRY, pagination_TF, pagination_SM)
import Control.Monad.State.Strict (StateT (StateT), runStateT, state, get, put)

import Control.Transition (Transition)
import Control.Pagination (PaginationTransition)

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

-- Now here are the tests (specifications) based on the examples above:

spec :: Spec
spec = do
    describe "Infinite effectless pagination" $ do
        describe "Both infinite state-jumps and finite state-jumps can have an infinite runtime" $ do
            it "God's infinite book with infinite state-jumps" $ do
                take 5 (infinite_pagination infiniteBook 1)    `shouldBe` ["Page #1", "Page #2", "Page #3", "Page #4", "Page #5"]
            it "A ticking clock with finite state-jumps but infinite runtime" $ do
                take 5 (infinite_pagination clockTicking True) `shouldBe` ["tick"   , "tock"   , "tick"   , "tock"   , "tick"   ]
    describe "Limitable effectless pagination" $ do
        describe "There are only four seasons, the sample is limited" $ do
            it "simple-function non-DRY solution" $ do
                pagination_nonDRY exemplifySeasons     `shouldBe` ["spring", "summer", "autumn", "winter"]
            it "simple-function but more economical (DRY) solution" $ do
                pagination_TF     exemplifySeasons     `shouldBe` ["spring", "summer", "autumn", "winter"]
            it "State-monad solution" $ do
                pagination_SM (state exemplifySeasons) `shouldBe` ["spring", "summer", "autumn", "winter"]
