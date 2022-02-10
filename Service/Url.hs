module Service.Url where


type URL          = String
type SearchPhrase = String

baseURL, mockBaseURL_contactError, mockBaseURL_formatError :: URL
baseURL                  = "https://en.wikipedia.org/w/api.php?action=query&format=json&list=search&utf8=1&srsearch="
mockBaseURL_contactError = "https://aaaaaaaaaaaaaaaa/w/api.php?action=query&format=json&list=search&utf8=1&srsearch=" -- wrong address
mockBaseURL_formatError  = "https://en.wikipedia.org/w/api.php?action=query&list=search&utf8=1&srsearch=" -- missing: &format=json 

searchURL :: SearchPhrase -> Maybe Int -> URL
searchURL = searchURL' baseURL

-- TODO: a searchphrase containing space must be encoded (quotated)


searchURL' :: URL -> SearchPhrase -> Maybe Int -> URL
searchURL' mockBaseURL searchPhrase mbToken = let basic = mockBaseURL ++ searchPhrase
                                                  pagin = maybe ""  ((++) "&sroffset=" . show) mbToken
                                              in basic ++ pagin
