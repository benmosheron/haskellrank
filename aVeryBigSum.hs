parse :: String -> Integer
parse = read 

main :: IO()
main = interact $ show . sum . map parse . words . last . lines
-- i accidentally started cahnging this and had to undo, it might be screwed up