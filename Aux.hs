module Aux where

import Control.Monad.State.Strict (State, StateT (StateT))

type Transition state value = state -> (value, state)
type PaginationTransition continuationToken page = Transition (Maybe continuationToken) page

type TransitionEffect state effect value = state -> effect (value, state)
type PaginationEffect token effect page  = TransitionEffect (Maybe token) effect page

type PaginationState token page = State (Maybe token) page
type PaginationStateT token effect page = StateT (Maybe token) effect page

pureTransitionFunctionToStateT :: Monad effect => Transition state value -> StateT state effect value
pureTransitionFunctionToStateT = StateT . (return .)
