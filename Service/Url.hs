module Service.Url where


baseURL :: String
baseURL = "https://en.wikipedia.org/w/api.php?action=query&format=json&list=search&utf8=1&srsearch="

searchURL :: String -> Maybe Int -> String
searchURL searchPhrase mbToken = let basic = baseURL ++ searchPhrase
                                     pagin = maybe ""  ((++) "&sroffset=" . show) mbToken
                                 in basic ++ pagin

-- TODO: a searchphrase containing space must be encoded (quotated)
