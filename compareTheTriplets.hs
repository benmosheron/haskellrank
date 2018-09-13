import Data.List

parseArray :: String -> [Int]
parseArray = map read . words

-- Our input is two Int arrays
getInput :: String -> [[Int]]
getInput = map parseArray . lines

-- Zip our two arrays, into list of F (first), S (second) or D for draw
data Res = F | S | D deriving (Eq,Show)

comp :: Int -> Int -> Res
comp first second
  | first > second = F
  | second > first = S
  | otherwise      = D

score :: [Int] -> [Int] -> [Res]
score first second = zipWith comp first second

-- Count our partitions Fs and Ss
lenTuple :: ([a], [a]) -> [Int]
lenTuple (fs, ss) = [length fs, length ss]

-- Ignoring draws, partition scores where the first wins from the second, and count each
calcResult :: [Int] -> [Int] -> [Int]
calcResult first second = lenTuple $ partition (== F) $ filter (/= D) $ score first second

compareTriplets :: [[Int]] -> [Int]
compareTriplets scores = let first = head scores
                             second = last scores
                             in calcResult first second

resultAsString :: [Int] -> String
resultAsString a = unwords $ map show a

main :: IO()
main = interact $ resultAsString . compareTriplets . getInput