import Data.List

-- Specifying the amount to take is nice because it means we don't have to ctrl+D to signal end of STDIN
readMatrix :: String -> [[Int]]
readMatrix = takeMatrix . map (map (read :: String -> Int) . words) . lines
  where takeMatrix m = take (head $ head m) $ tail m

-- Add the top and bottom surface areas to the accumulated vertical change over the x and y dimensions
surfaceArea :: [[Int]] -> Int
surfaceArea m = (x*y)*2 + (runningSA m) + (runningSA $ transpose m)
  where x = length $ head m
        y = length m
        runningSA m' = sum $ map (surface1D 0 0) m'

surface1D :: Int -> Int -> [Int] -> Int
surface1D total' prev [] = total' + (abs prev)
surface1D total' prev v = surface1D (total' + (abs $ prev - h)) h (tail v) where h = head v

main :: IO()
main = interact $ show . surfaceArea . readMatrix
