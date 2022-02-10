{-# LANGUAGE OverloadedStrings #-}

module Service.ServiceLow (ServiceLow_unsafe, theServiceLow_unsafe, serviceFormatErrorMsg, ServiceLow, theServiceLow, ServiceLow', theServiceLow') where

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

type ServiceLow_unsafe = String -> IO JSONResponseObject

theServiceLow_unsafe :: ServiceLow_unsafe
theServiceLow_unsafe = fmap (fromMaybe $ error serviceFormatErrorMsg) . theServiceLow

---
-- Total ones:
---

serviceConnectionErrorMsg, serviceFormatErrorMsg, serviceUnspecifiedErrorMsg :: String
serviceConnectionErrorMsg  = "There is a connection error to the target service"
serviceFormatErrorMsg      = "The target service responds in an incompatible format"
serviceUnspecifiedErrorMsg = "Unspecified service error. Either: " ++ serviceConnectionErrorMsg ++ ". Or: " ++ serviceFormatErrorMsg ++ "."

--lengthy (total, but without the monad transformer formalism)
type ServiceLow = String -> IO (Maybe JSONResponseObject)

theServiceLow :: ServiceLow
theServiceLow url = do
    resp <- get url
    let rawBody = resp ^. responseBody
    let maybeJsonObject = decode rawBody
    return maybeJsonObject


-- modular (total, and with with the monad transformer formalism)
type ServiceLow' = String -> MaybeT IO JSONResponseObject

theServiceLow' :: ServiceLow'
theServiceLow' =  MaybeT . theServiceLow -- = lift . theServiceLow_unsafe
