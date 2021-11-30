{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Network.Wreq

import Control.Lens
import GHC.Generics
import Data.Aeson.Lens (_String, key)
import Data.Aeson (Value)
-- import Data.Aeson.Types

import Data.Aeson.Internal

import Data.Map

-- import Prelude hiding (putStrLn)
import Data.ByteString.Lazy.Char8 as B

import Data.ByteString.Lazy

type Resp = Response (Map String Value)

main :: IO ()
main = do
    resp <- get "https://en.wikipedia.org/w/api.php?action=query&format=json&list=search&utf8=1&srsearch=Nelson%20Mandela"
    -- asJSON resp :: IO (Response (Map String Value))
    -- let sc = resp ^. responseStatus . statusCode
    let bd = resp ^. responseBody
    -- asJSON resp >>= suck
    B.putStrLn bd


suck :: Response (Map String Value) -> IO ()
suck _ = return ()
