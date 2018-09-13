-- Parse a single int from a string, e.g. "1" = 1
parse :: String -> Int
parse = read 

-- Our input ignores the first line, the second line is space separated ints
getInput :: String -> [Int]
getInput = map parse . words . head . tail . lines

-- Built in sum function
simpleArraySum :: [Int] -> Int
simpleArraySum = sum

main :: IO()
main = interact $ show . simpleArraySum . getInput