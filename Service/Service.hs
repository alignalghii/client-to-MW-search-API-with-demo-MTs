{-# LANGUAGE OverloadedStrings #-}

module Service.Service (Service_unsafe, callService_unsafe, serviceFormatErrorMsg, Service, callService, Service', callService') where

import Service.InterpretJSON (JSONResponseObject)

import Network.Wreq (get, responseBody)
import Control.Lens ((^.))
import Data.Aeson (decode)

-- import Data.Text (Text)
import Prelude hiding (putStrLn)
import Data.ByteString.Lazy.Char8 (pack, unpack, putStrLn)

import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))

---
-- unsafe (partial)
--

type Service_unsafe = String -> IO JSONResponseObject

callService_unsafe :: Service_unsafe
callService_unsafe = fmap (fromMaybe $ error serviceFormatErrorMsg) . callService

---
-- Total ones:
---

serviceConnectionErrorMsg, serviceFormatErrorMsg, serviceUnspecifiedErrorMsg :: String
serviceConnectionErrorMsg  = "There is a connection error to the target service"
serviceFormatErrorMsg      = "The target service responds in an incompatible format"
serviceUnspecifiedErrorMsg = "Unspecified service error. Either: " ++ serviceConnectionErrorMsg ++ ". Or: " ++ serviceFormatErrorMsg ++ "."

--lengthy (total, but without the monad transformer formalism)
type Service = String -> IO (Maybe JSONResponseObject)

callService :: Service
callService url = do
    resp <- get url
    let rawBody = resp ^. responseBody
    let maybeJsonObject = decode rawBody
    return maybeJsonObject


-- modular (total, and with with the monad transformer formalism)
type Service' = String -> MaybeT IO JSONResponseObject

callService' :: Service'
callService' =  MaybeT . callService -- = lift . callService_unsafe
