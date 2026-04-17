allWordsStartSameLetter :: String -> Bool
allWordsStartSameLetter str =
    let wordsList = myWords str
    in allSameFirstLetter wordsList
    where
        myToLower c
            | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
            | otherwise = c
        
        myWords :: String -> [String]
        myWords [] = []
        myWords (c:cs)
            | c == ' ' = myWords cs
            | otherwise = myWords' cs [c] []
        
        myWords' :: String -> String -> [String] -> [String]
        myWords' [] current result = result ++ [current]
        myWords' (c:cs) current result
            | c == ' ' = myWords' cs [] (result ++ [current])
            | otherwise = myWords' cs (current ++ [c]) result
        
        allSameFirstLetter :: [String] -> Bool
        allSameFirstLetter [] = True
        allSameFirstLetter [w] = True
        allSameFirstLetter (w1:w2:ws) =
            let firstLetter1 = myToLower (head w1)
                firstLetter2 = myToLower (head w2)
            in firstLetter1 == firstLetter2 && allSameFirstLetter (w2:ws)