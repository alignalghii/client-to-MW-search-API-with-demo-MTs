{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wreq

import Control.Lens
import Data.Aeson.Lens (_String, key)
import Data.Aeson (Value, decode, encode)

import Data.Map

import Prelude hiding (putStrLn)
import Data.ByteString.Lazy.Char8 (unpack, putStrLn)

-- import Data.ByteString.Lazy (unpack)

type Resp = Response (Map String Value)

main :: IO ()
main = do
    resp <- get "https://en.wikipedia.org/w/api.php?action=query&format=json&list=search&utf8=1&srsearch=Haskell"
    -- asJSON resp :: IO (Response (Map String Value))
    -- let sc = resp ^. responseStatus . statusCode
    let rawBody = resp ^. responseBody
    let rawBodySimpleString = unpack rawBody
    let Just ast = decode rawBody :: Maybe Value
    putStrLn $ encode ast
    -- let sh = fromJust ma
    -- asJSON resp >>= suck
    -- putStrLn rawBodySimpleString
    -- print ast


suck :: Response (Map String Value) -> IO ()
suck _ = return ()
