import Data.List

parseLine :: String -> ([Int],[Int])
parseLine s = (map read . drop 2 $ words l, map read $ words r)
  where (l,_:r) = span ('|'/=) s

numInts :: ([Int],[Int]) -> Int
numInts = length . uncurry intersect

score1 :: ([Int],[Int]) -> Int
score1 lr
    | len == 0  = 0
    | otherwise = 2 ^ (len - 1)
  where len = numInts lr

recurse :: [Int] -> Int
recurse [] = 0
recurse (x:xs) = (1+) . sum . map recurse . take x $ tails xs

part1 :: IO Int
part1 = sum . map (score1 . parseLine) . lines <$> readFile "Day_4/input.txt"

part2 :: IO Int
part2 = sum . map recurse . tails . map (numInts . parseLine) . lines <$> readFile "Day_4/input.txt"