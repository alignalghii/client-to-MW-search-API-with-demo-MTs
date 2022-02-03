{-# LANGUAGE OverloadedStrings #-}

module Service.Service (Service, callService) where

import Service.InterpretJSON (JSONResponseObject)

import Network.Wreq (get, responseBody)
import Control.Lens ((^.))
import Data.Aeson (decode)

-- import Data.Text (Text)
import Prelude hiding (putStrLn)
import Data.ByteString.Lazy.Char8 (pack, unpack, putStrLn)


type Service = String -> IO (Maybe JSONResponseObject)

callService :: Service
callService url = do
    resp <- get url
    let rawBody = resp ^. responseBody
    let maybeJsonObject = decode rawBody
    return maybeJsonObject
