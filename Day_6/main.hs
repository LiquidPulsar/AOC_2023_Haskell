import Data.Char

parseLine :: String -> [Double]
parseLine = map read . words . dropWhile (not . isDigit)

parseLine2 :: String -> Double
parseLine2 = read . concat . words . dropWhile (not . isDigit)

parse :: (String -> a) -> String -> (a,a)
parse f s = let [a,b] = lines s in (f a, f b)

wins :: Double -> Double -> Int
wins t d = let
    s = sqrt (t^2 - 4*d)
    l = (t - s)/2 + 1e-9 -- Roots on exact boundary, so shift by epsilon for >
    r = (t + s)/2 - 1e-9
    in floor r - ceiling l + 1

part1 :: IO Int
part1 = product . uncurry (zipWith wins) . parse parseLine <$> readFile "Day_6/input.txt"

part2 :: IO Int
part2 = uncurry wins . parse parseLine2 <$> readFile "Day_6/input.txt"