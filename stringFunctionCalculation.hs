-- It's bad... takes too long. Use a suffix tree to win.
import Data.List
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

-- Count every substring's occurrences
calcAll :: String -> Map String Int
calcAll orig = Map.mapWithKey (\k v -> length k * v) $ foldl addOrInc (Map.empty) $ subStrings orig

subStrings :: String -> [String]
subStrings = filter (/= "") . concat . (map tails) . inits

addOrInc :: Map String Int -> String -> Map String Int
addOrInc m' sub = Map.insertWith (+) sub 1 m'

calc :: Map String Int -> Int
calc m = maximum $ map snd (Map.toList m)

main :: IO()
main = interact $ show . calc . calcAll . head . lines