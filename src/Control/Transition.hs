module Control.Transition where


type Transition state value = state -> (value, state)

type TransitionEffect state effect value = state -> effect (value, state)
