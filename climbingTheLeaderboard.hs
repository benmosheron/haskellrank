import Data.List
import Data.Maybe

type C = (Int,Int)

rankAndChop :: C -> [[Int]] -> Int -> (Int,C,[[Int]])
-- no leader board left, we must be last
rankAndChop (curr,max) [] _ = (max+1,(curr,max),[])
rankAndChop (curr,max) lb s
  -- we are better than or equal to the top score, we must be curr. Maintain curr and pass lb unchanged.
  | s >= top = (curr,(curr,max),lb) 
  -- we are lower than the top score, we are worse than curr, move down the lb
  | s < top = rankAndChop (curr+1,max) (tail lb) s
 where top = head $ head lb

-- we need to supply the scores from highest to lowest (reversed)
ranksRec :: C -> [[Int]] -> [Int] -> [Int] -> [Int]
ranksRec _ _ [] results'      = results'
ranksRec c lb (s:ss) results' = ranksRec c' lb' ss (r : results')
                                where
                                  (r,c',lb') = rankAndChop c lb s

start :: [Int] -> [Int] -> [Int]
start lb s = ranksRec (1,length gb) gb (reverse s) []
 where gb = group lb

-- We only need the 2nd and 4th lines
getInputs :: [String] -> ([Int],[Int])
getInputs (_:lbs:_:ss:[]) = (parse lbs, parse ss) where parse = map (read :: String -> Int) . words

calculate :: ([Int],[Int]) -> String
calculate (lb, scores) = unlines $ map show $ start lb scores

main :: IO()
main = interact $ calculate . getInputs . lines