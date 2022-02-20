module Control.Pagination where

import Control.Transition (Transition, TransitionEffect)
import Control.Monad.State.Strict (State, StateT)


type PaginationTransition continuationToken page = Transition (Maybe continuationToken) page

type PaginationEffect token effect page  = TransitionEffect (Maybe token) effect page

type PaginationState token page = State (Maybe token) page
type PaginationStateT token effect page = StateT (Maybe token) effect page
