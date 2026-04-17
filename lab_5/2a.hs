func :: Integer -> Bool
func n | n < 100^3 = False
       | otherwise = length [() | a <- [100..999], 
                                  b <- [100..999], 
                                  let c = n `div` (a * b),
                                  c >= 100 && c <= 999 && a * b * c == n] > 0