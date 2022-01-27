module Data.ErrorEffect where

type ErrorEffect = Either ErrorMsg

type ErrorMsg = String

errorfree :: val    -> ErrorEffect val
errorfree = Right

withError :: String -> ErrorEffect val
withError = Left
