module MetaFeatures.IntegrationTest (runIntegrationTests) where

import Service.WebServiceConsultation (searchMessage)
import Service.ServiceLow (serviceFormatErrorMsg)
import Service.Url (URL, mockBaseURL_formatError)
import System.IO (hFlush, stdout)


runIntegrationTests :: IO ()
runIntegrationTests = do
    putStrLn "Integration tests:"
    putStr   "  -  Invalid JSON format response from server is handler by this client correctly: "
    flush
    message <- searchMessage mockBaseURL_formatError "Haskell"
    checking (message == serviceFormatErrorMsg)


checking :: Bool -> IO ()
checking flag = putStrLn $ show flag ++ "\r  " ++ [symbol flag]

symbol :: Bool -> Char
symbol True  = '✓'
symbol False = '✕'

flush :: IO ()
flush = hFlush stdout
