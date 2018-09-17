hurdleRace :: [[Int]] -> String
hurdleRace ((_:k:[]):hurdles:[]) = show $ max 0 $ maximum hurdles - k

main :: IO()
main = interact $ hurdleRace . map (map (read :: String -> Int) . words) . lines