import Data.List

main :: IO ()
main = print . solve =<< getLine

solve :: String -> Integer
solve str = sum [read [a] | (a,b) <- rotate str, a == b]

rotate :: [a] -> [(a,a)]
rotate (x:xs) = zip (x:xs) (xs++[x])
