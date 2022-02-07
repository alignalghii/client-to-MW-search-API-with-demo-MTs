module PaginationStateMachines.Effectful.PaginationConcept_lazy (pagination_MT_lazy) where

import Control.Pagination_lazy (PaginationStateT)

import Control.Monad.State.Lazy (get, evalStateT)
import Control.Monad (liftM2)
import Data.Maybe (isJust)


-- https://stackoverflow.com/questions/14494648/why-the-haskell-sequence-function-cant-be-lazy-or-why-recursive-monadic-functio

----------------------------------------------------------------------
-- Building a monadic effect on top of a state tansition
--  * either upon a transition function as a monadic wrapper parameter
--  * or upon a state monad transformer
----------------------------------------------------------------------



-- Implemented *with* monad transformers:

pagination_MT_lazy :: Monad effect => PaginationStateT pgnToken effect page -> effect [page]
pagination_MT_lazy = flip evalStateT Nothing . pagination_MT_lazy' True

pagination_MT_lazy' :: Monad effect => Bool -> PaginationStateT pgnToken effect page -> PaginationStateT pgnToken effect [page]
pagination_MT_lazy' isStartMode transitEffect = do
    maybePgnToken <- get
    if isStartMode || isJust maybePgnToken
        then liftM2 (:) transitEffect $ pagination_MT_lazy' False transitEffect
        else return []
