module Effectful.PaginationConceptSeries where

import SampleTypes

import Control.Monad.State.Strict
import Data.Maybe (isJust)

import Aux


pagination_noMT :: Monad effect => PaginationEffect pgnToken effect page -> effect [page]
pagination_noMT transitEffect = pagination_noMT' True transitEffect Nothing

pagination_noMT' :: Monad effect => Bool -> PaginationEffect pgnToken effect page -> Maybe pgnToken -> effect [page]
pagination_noMT' isStartMode transitEffect maybePgnToken = do
    if isStartMode || isJust maybePgnToken
        then do
            (firstPage, maybeNextPgnToken) <- transitEffect maybePgnToken
            (firstPage :) <$> pagination_noMT' False transitEffect maybeNextPgnToken
        else return []

pagination_MT :: Monad effect => PaginationStateT pgnToken effect page -> effect [page]
pagination_MT = flip evalStateT Nothing . pagination_MT' True

pagination_MT' :: Monad effect => Bool -> PaginationStateT pgnToken effect page -> PaginationStateT pgnToken effect [page]
pagination_MT' isStartMode transitEffect = do
    maybePgnToken <- get
    if isStartMode || isJust maybePgnToken
        then liftM2 (:) transitEffect $ pagination_MT' False transitEffect
        else return []
