module Effectful.PaginationConceptSeries where

import SampleTypes

import Control.Monad.State.Strict
import Data.Maybe (isJust)

import Aux


effect_pagination_noMT :: Monad effect => PaginationEffect pgnToken effect page -> effect [page]
effect_pagination_noMT transitEffect = effect_pagination_noMT' True transitEffect Nothing

effect_pagination_noMT' :: Monad effect => Bool -> PaginationEffect pgnToken effect page -> Maybe pgnToken -> effect [page]
effect_pagination_noMT' isStartMode transitEffect maybePgnToken = do
    if isStartMode || isJust maybePgnToken
        then do
            (firstPage, maybeNextPgnToken) <- transitEffect maybePgnToken
            (firstPage :) <$> effect_pagination_noMT' False transitEffect maybeNextPgnToken
        else return []

effect_pagination_MT :: Monad effect => PaginationStateT pgnToken effect page -> effect [page]
effect_pagination_MT = flip evalStateT Nothing . effect_pagination_MT' True

effect_pagination_MT' :: Monad effect => Bool -> PaginationStateT pgnToken effect page -> PaginationStateT pgnToken effect [page]
effect_pagination_MT' isStartMode transitEffect = do
    maybePgnToken <- get
    if isStartMode || isJust maybePgnToken
        then liftM2 (:) transitEffect $ effect_pagination_MT' False transitEffect
        else return []
