myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:xs) = let maxRest = myMaximum xs in if x > maxRest then x else maxRest

averageOfMaxima :: [[Int]] -> Double
averageOfMaxima lists = 
    let (sumMax, count) = foldr processMax (0, 0) lists
    in (sumMax' + 0.0) / count'
    where
        processMax xs (accSum, accCount) = 
            let maxVal = myMaximum xs
            in (accSum + maxVal, accCount + 1)
        
        sumMax' = sumMax + 0.0
        count' = count + 0.0