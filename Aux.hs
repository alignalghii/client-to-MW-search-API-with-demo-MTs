module Aux where

import Control.Monad.State.Strict (State, StateT (StateT))

type Transition state value = state -> (value, state)
type PaginationTransition continuationToken page = Transition (Maybe continuationToken) page

type TransitionProcess state process value = state -> process (value, state)
type PaginationProcess token process page  = TransitionProcess (Maybe token) process page

type Pagination token page = State (Maybe token) page
type PaginationT token process page = StateT (Maybe token) process page

pureTransitionFunctionToStateT :: Monad process => Transition state value -> StateT state process value
pureTransitionFunctionToStateT = StateT . (return .)
