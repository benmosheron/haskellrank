-- not finished, think a better approach would be to generate all possible magic squares (9!)
-- and find the shortet distance to one of them

import Data.List
import Data.Maybe

-- Let's do some investigating
parseSquare :: String -> [[Int]]
parseSquare =  map (map (read :: String -> Int) . words) . lines

-- we have all digits 1 to 9
noRepeats :: [[Int]] -> Bool
noRepeats s = all (\e -> elem e c) [1..9] where c = concat s

-- Build all our equality groups
groups :: [[Int]] -> [[Int]]
groups s = 
    rows ++                                                           -- Rows
    (transpose rows) ++                                               -- Columns
    [[ s !! i !! i | i <- [0..2]],[ s !! i !! (2 - i) | i <- [0..2]]] -- Diagonals
  where 
    rows = [ [ s !! i !! j | j <- [0..2]] | i <- [0..2]]

sumTo15 :: [[Int]] -> Bool
sumTo15 s = all (== 15) sums where sums = map sum $ groups s

isMagic :: [[Int]] -> Bool
isMagic s = noRepeats s && sumTo15 s

isMagicDbg :: String -> String
isMagicDbg s = (show $ isMagic sq) ++ (show $ sums) ++ "\n" 
  where sq = parseSquare s
        sums = map sum $ groups $ sq

main :: IO()
main = interact isMagicDbg