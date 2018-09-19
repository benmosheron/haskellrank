fac :: Integer -> Integer -> Integer
fac t' 1 = t'
fac t' x = fac (t' * x) (x - 1) 

main :: IO()
main = interact $  show . (fac 1) . (read :: String -> Integer)