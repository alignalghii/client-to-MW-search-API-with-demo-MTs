module Effectless.PaginationConceptSeries where

import Data.Transition (Transition)
import Data.Pagination (PaginationTransition, PaginationState)

import Control.Monad.State.Strict
import Data.Maybe (isJust)


infinite_pagination :: Transition paginationState page -> paginationState -> [page]
infinite_pagination transitFun initialState = let (firstPage, nextState) = transitFun initialState
                                              in firstPage : infinite_pagination transitFun nextState

pagination_nonDRY :: PaginationTransition pgnToken page -> [page]
pagination_nonDRY = flip pagination_nonDRY' Nothing

pagination_nonDRY', pagination_nonDRY_end :: PaginationTransition pgnToken page -> Maybe pgnToken -> [page]
pagination_nonDRY' transitFun maybePgnToken = let (firstPage, maybeNextPgnToken) = transitFun maybePgnToken
                                              in firstPage : pagination_nonDRY_end transitFun maybeNextPgnToken
pagination_nonDRY_end transitFun maybePgnToken = case maybePgnToken of
                                                            Nothing       -> []
                                                            Just pgnToken -> let (page, maybeNextPgnToken) = transitFun maybePgnToken
                                                                             in page : pagination_nonDRY_end transitFun maybeNextPgnToken

pagination :: PaginationTransition pgnToken page -> [page]
pagination transitFun = pagination' True transitFun Nothing

pagination' :: Bool -> PaginationTransition pgnToken page -> Maybe pgnToken -> [page]
pagination' isStartMode transitFun maybePgnToken
    | isStartMode || isJust maybePgnToken = let (firstPage, maybeNextPgnToken) = transitFun maybePgnToken
                                            in firstPage : pagination' False transitFun maybeNextPgnToken
    | otherwise                           = []

pagination_SM :: PaginationState pgnToken page -> [page]
pagination_SM = flip evalState Nothing . pagination_SM' True

pagination_SM' :: Bool -> PaginationState pgnToken page -> PaginationState pgnToken [page]
pagination_SM' isStartMode transition = do
        maybeToken <- get
        if isStartMode || isJust maybeToken
            then liftM2 (:) transition $ pagination_SM' False transition
            else return []
