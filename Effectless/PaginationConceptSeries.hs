module Effectless.PaginationConceptSeries where

import SampleTypes

import Control.Monad.State.Strict
import Data.Maybe (isJust)

import Aux

infinite_native_pagination :: Transition paginationState page -> paginationState -> [page]
infinite_native_pagination transitFun initialState = let (firstPage, nextState) = transitFun initialState
                                                     in firstPage : infinite_native_pagination transitFun nextState

native_pagination_nonDRY :: PaginationTransition pgnToken page -> [page]
native_pagination_nonDRY = flip native_pagination_nonDRY' Nothing

native_pagination_nonDRY', native_pagination_nonDRY_end :: PaginationTransition pgnToken page -> Maybe pgnToken -> [page]
native_pagination_nonDRY' transitFun maybePgnToken = let (firstPage, maybeNextPgnToken) = transitFun maybePgnToken
                                                     in firstPage : native_pagination_nonDRY_end transitFun maybeNextPgnToken
native_pagination_nonDRY_end transitFun maybePgnToken = case maybePgnToken of
                                                            Nothing       -> []
                                                            Just pgnToken -> let (page, maybeNextPgnToken) = transitFun maybePgnToken
                                                                             in page : native_pagination_nonDRY_end transitFun maybeNextPgnToken

native_pagination :: PaginationTransition pgnToken page -> [page]
native_pagination transitFun = native_pagination' True transitFun Nothing

native_pagination' :: Bool -> PaginationTransition pgnToken page -> Maybe pgnToken -> [page]
native_pagination' isStartMode transitFun maybePgnToken
    | isStartMode || isJust maybePgnToken = let (firstPage, maybeNextPgnToken) = transitFun maybePgnToken
                                            in firstPage : native_pagination' False transitFun maybeNextPgnToken
    | otherwise                           = []

native_pagination_SM :: PaginationState pgnToken page -> [page]
native_pagination_SM = flip evalState Nothing . native_pagination_SM' True

native_pagination_SM' :: Bool -> PaginationState pgnToken page -> PaginationState pgnToken [page]
native_pagination_SM' isStartMode transition = do
        maybeToken <- get
        if isStartMode || isJust maybeToken
            then liftM2 (:) transition $ native_pagination_SM' False transition
            else return []
