{-# LANGUAGE OverloadedStrings #-}

module Service.ServiceLow (ServiceLow_unsafe, theServiceLow_unsafe, serviceFormatErrorMsg, ServiceLow_lengthy, theServiceLow_lengthy, ServiceLow, theServiceLow) where

import Service.InterpretJSON (JSONResponseObject)
import Service.Url (URL)

import Network.Wreq (get, responseBody)
import Control.Lens ((^.))
import Data.Aeson (decode)

-- import Data.Text (Text)
import Prelude hiding (putStrLn)
import Data.ByteString.Lazy.Char8 (pack, unpack, putStrLn)

import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))

---
-- Unsafe (partial) approach:
--

type ServiceLow_unsafe = URL -> IO JSONResponseObject

theServiceLow_unsafe :: ServiceLow_unsafe
theServiceLow_unsafe = fmap (fromMaybe $ error serviceFormatErrorMsg) . theServiceLow_lengthy
-- Code smell: don't hide service call errors! Redesign, or at least extend the monad transformer stack. See below.

---
-- Total (safe) solutions:
---

serviceConnectionErrorMsg, serviceFormatErrorMsg, serviceUnspecifiedErrorMsg :: String
serviceConnectionErrorMsg  = "There is a connection error to the target service"
serviceFormatErrorMsg      = "The target service responds in an incompatible format"
serviceUnspecifiedErrorMsg = "Unspecified service error. Either: " ++ serviceConnectionErrorMsg ++ ". Or: " ++ serviceFormatErrorMsg ++ "."

-- Lengthy (total, but without the monad transformer formalism)
type ServiceLow_lengthy = URL -> IO (Maybe JSONResponseObject)

theServiceLow_lengthy :: ServiceLow_lengthy
theServiceLow_lengthy url = do
    resp <- get url
    let rawBody = resp ^. responseBody
    let maybeJsonObject = decode rawBody
    return maybeJsonObject


-- Modular (total, and with with the monad transformer formalism)
type ServiceLow = URL -> MaybeT IO JSONResponseObject

theServiceLow :: ServiceLow
theServiceLow =  MaybeT . theServiceLow_lengthy -- = lift . theServiceLow_unsafe
