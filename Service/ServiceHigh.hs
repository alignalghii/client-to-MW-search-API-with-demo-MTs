module Service.ServiceHigh (printHigh, showHigh, abstractFromLowRepresentation, abstractFromLowRepresentation_withMockBase) where

import Control.Pagination (PaginationEffect)
import Service.ServiceLow (Service', callService', serviceFormatErrorMsg)
import Service.Url (URL, searchURL, searchURL')
import Service.SearchResult (SearchResult, showSearchResult, Title, Sroffset)
import Service.InterpretJSON (extractFoundTitlesAndSroffset)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)


printHigh :: MaybeT IO SearchResult -> IO ()
printHigh = (=<<) (putStrLn . showHigh) . runMaybeT

showHigh :: Maybe SearchResult -> String
showHigh = maybe serviceFormatErrorMsg showSearchResult

abstractFromLowRepresentation :: String -> MaybeT IO SearchResult
abstractFromLowRepresentation searchphrase = abstractFromLowRepresentation' callService' searchphrase Nothing

abstractFromLowRepresentation' :: Service' -> String -> PaginationEffect Sroffset (MaybeT IO) [Title]
abstractFromLowRepresentation' service' searchphrase = fmap extractFoundTitlesAndSroffset . service' . searchURL searchphrase
--    jsonResponseObject <- service' $ searchURL searchphrase maybeSroffset
--    return $ extractFoundTitlesAndSroffset jsonResponseObject
-- MaybeT . fmap (fmap extractFoundTitlesAndSroffset) . service . searchURL searchphrase
-- wrapperE service searchphrase maybeSroffset = do
--     maybeJsonResponseObject <- lift $ service $ searchURL searchphrase maybeSroffset
--     MaybeT $ return $ fmap extractFoundTitlesAndSroffset maybeJsonResponseObjectSroffset

abstractFromLowRepresentation_withMockBase :: URL -> String -> PaginationEffect Sroffset (MaybeT IO) [Title]
abstractFromLowRepresentation_withMockBase mockBaseURL searchphrase = fmap extractFoundTitlesAndSroffset . callService' . searchURL' mockBaseURL searchphrase
