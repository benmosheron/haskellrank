parse :: String -> Int
parse = read 

-- Get the indices of a list
is :: [a] -> [Int]
is a = [0..(length a - 1)]

-- Get the sum of the elements at indices generated from a by g
f :: ([[Int]] -> [Int]) -> [[Int]] -> Int
f g a = sum $ map (\(i,xs) -> xs !! i) (zip (g a) a)

ldiag :: [[Int]] -> Int
ldiag = f is

rdiag :: [[Int]] -> Int
rdiag = f (reverse . is)

diags :: [[Int]] -> Int
diags a = abs $ (ldiag a) - (rdiag a)

main :: IO()
main = interact $ show . diags . map (map parse . words) . tail . lines