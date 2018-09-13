parse :: String -> Int
parse = read 

-- length of b over length of a
lengthRatio :: [Int] -> [Int] -> Double
lengthRatio a b = fromIntegral (length b) / fromIntegral (length a)

-- sort into positives, negatives and zeros, calculate the length ratios and stringify
plusMinusZero :: [Int] -> String
plusMinusZero a = unlines $ map (show . lengthRatio a) [ps,ns,zs] 
                  where ps = filter (> 0) a
                        ns = filter (< 0) a
                        zs = filter (== 0) a

main :: IO()
main = interact $ plusMinusZero . map parse . words . last . lines