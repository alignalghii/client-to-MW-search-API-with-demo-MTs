module Effectful.PaginationConceptSeries (pagination_noMT, pagination_MT) where

import Control.Pagination (PaginationEffect, PaginationStateT)

import Control.Monad.State.Strict (get, evalStateT)
import Control.Monad (liftM2)
import Data.Maybe (isJust)


----------------------------------------------------------------------
-- Building a monadic effect on top of a state tansition
--  * either upon a transition function as a monadic wrapper parameter
--  * or upon a state monad transformer
----------------------------------------------------------------------


-- Implemented *without* monad transformers:

pagination_noMT :: Monad effect => PaginationEffect pgnToken effect page -> effect [page]
pagination_noMT transitEffect = pagination_noMT' True transitEffect Nothing

pagination_noMT' :: Monad effect => Bool -> PaginationEffect pgnToken effect page -> Maybe pgnToken -> effect [page]
pagination_noMT' isStartMode transitEffect maybePgnToken = do
    if isStartMode || isJust maybePgnToken
        then do
            (firstPage, maybeNextPgnToken) <- transitEffect maybePgnToken
            (firstPage :) <$> pagination_noMT' False transitEffect maybeNextPgnToken
        else return []


-- Implemented *with* monad transformers:

pagination_MT :: Monad effect => PaginationStateT pgnToken effect page -> effect [page]
pagination_MT = flip evalStateT Nothing . pagination_MT' True

pagination_MT' :: Monad effect => Bool -> PaginationStateT pgnToken effect page -> PaginationStateT pgnToken effect [page]
pagination_MT' isStartMode transitEffect = do
    maybePgnToken <- get
    if isStartMode || isJust maybePgnToken
        then liftM2 (:) transitEffect $ pagination_MT' False transitEffect
        else return []
