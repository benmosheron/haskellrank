import Data.List
import Data.Maybe

-- Rank a score s in a leaderboard lb
rank :: [Int] -> Int -> Int
rank lb s = 1 + zbRank
  where gb = group lb
        zbRank = fromMaybe (length gb) $ findIndex ((<= s) . head) (gb)

ranks :: [Int] -> [Int] -> [Int]
ranks lb scores = map (rankEfficient (length lb) (group lb)) scores

rankEfficient :: Int -> [[Int]] -> Int -> Int
rankEfficient n gb s = 1 + (fromMaybe (length gb) $ findIndex ((<= s) . head) (gb))

-- We only need the 2nd and 4th lines
getInputs :: [String] -> ([Int],[Int])
getInputs (_:lbs:_:ss:[]) = (parse lbs, parse ss) where parse = map (read :: String -> Int) . words

calculate :: ([Int],[Int]) -> String
calculate (lb, scores) = unlines $ map show $ ranks lb scores

main :: IO()
main = interact $ calculate . getInputs . lines