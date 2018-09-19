import Data.List

query :: [Int] -> Int -> [Int]
query q ai = foldl (\acc qi -> (qi + head acc):acc) ([ai + head q]) (tail q)


calcWithTranspose :: ([Int],[Int]) -> [Int]
calcWithTranspose (a,q) = reverse $ map (sum . map abs) $ transpose $ map (query q) a

getInput :: String -> ([Int],[Int])
getInput s =  (parse a, parse q)
  where (_:a:_:q:[]) = take 4 $ lines s
        parse = map (read :: String -> Int) . words

main :: IO()
main = interact $ unlines . map show . calcWithTranspose . getInput