uniqueDigits :: Integer -> [Int]
uniqueDigits n = removeDuplicates (getDigits (abs n))
  where
    getDigits 0 = [0]
    getDigits num = getDigitsHelper num []
    
    getDigitsHelper 0 acc = acc
    getDigitsHelper num acc = 
        let digit = fromInteger (num `mod` 10)
        in getDigitsHelper (num `div` 10) (digit : acc)
    
    removeDuplicates [] = []
    removeDuplicates (x:xs) = 
        if isElement x xs
        then removeDuplicates xs
        else x : removeDuplicates xs
    
    isElement _ [] = False
    isElement e (y:ys) = e == y || isElement e ys