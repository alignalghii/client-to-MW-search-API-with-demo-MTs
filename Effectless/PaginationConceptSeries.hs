module Effectless.PaginationConceptSeries where

import Data.Transition (Transition)
import Data.Pagination (PaginationTransition, PaginationState)

import Control.Monad.State.Strict
import Data.Maybe (isJust)

----------------------------------------------------------------------------------------
-- Didactic enumeration of simpified pagination-like functions in increasing developent,
-- the final one being the practically most usable (State-)monadic `pagination_SM`.
----------------------------------------------------------------------------------------

-- Pagination wthout termination handling (infinite pagination):

infinite_pagination :: Transition paginationState page -> paginationState -> [page]
infinite_pagination transitFun initialState = let (firstPage, nextState) = transitFun initialState
                                              in firstPage : infinite_pagination transitFun nextState

-- Pagination with termination handling --- some redundancy in implementation (non-DRY):

pagination_nonDRY :: PaginationTransition pgnToken page -> [page]
pagination_nonDRY = flip pagination_nonDRY' Nothing

pagination_nonDRY', pagination_nonDRY_end :: PaginationTransition pgnToken page -> Maybe pgnToken -> [page]
pagination_nonDRY' transitFun maybePgnToken = let (firstPage, maybeNextPgnToken) = transitFun maybePgnToken
                                              in firstPage : pagination_nonDRY_end transitFun maybeNextPgnToken
pagination_nonDRY_end transitFun maybePgnToken = case maybePgnToken of
                                                            Nothing       -> []
                                                            Just pgnToken -> let (page, maybeNextPgnToken) = transitFun maybePgnToken
                                                                             in page : pagination_nonDRY_end transitFun maybeNextPgnToken


-- Pagination with termination handling and non-redundant (DRY) implementation,
-- it does not use monads (State monad) yet, instead, transition function is managed directly:

pagination_TF :: PaginationTransition pgnToken page -> [page]
pagination_TF transitionFunction = pagination_TF' True transitionFunction Nothing

pagination_TF' :: Bool -> PaginationTransition pgnToken page -> Maybe pgnToken -> [page]
pagination_TF' isStartMode transitionFunction maybePgnToken
    | isStartMode || isJust maybePgnToken = let (firstPage, maybeNextPgnToken) = transitionFunction maybePgnToken
                                            in firstPage : pagination_TF' False transitionFunction maybeNextPgnToken
    | otherwise                           = []

-------------------------------------------------------------------------------
-- Pagination with termination handling and non-redundant (DRY) implementation,
-- it uses monads: namely the (strict) `State` monad.
-- Seems to be the most practical, the most developed one:
-------------------------------------------------------------------------------

pagination_SM :: PaginationState pgnToken page -> [page]
pagination_SM = flip evalState Nothing . pagination_SM' True

pagination_SM' :: Bool -> PaginationState pgnToken page -> PaginationState pgnToken [page]
pagination_SM' isStartMode transition = do
        maybeToken <- get
        if isStartMode || isJust maybeToken
            then liftM2 (:) transition $ pagination_SM' False transition
            else return []
