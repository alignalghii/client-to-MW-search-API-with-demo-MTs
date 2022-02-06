module Control.Pagination_lazy (PaginationState, PaginationStateT) where

import Control.Transition (Transition, TransitionEffect)
import Control.Monad.State.Lazy (State, StateT)


type PaginationState token page = State (Maybe token) page
type PaginationStateT token effect page = StateT (Maybe token) effect page
