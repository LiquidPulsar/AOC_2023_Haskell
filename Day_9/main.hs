diff :: [Int] -> [Int]
diff xs = zipWith (-) (tail xs) xs

toStack :: [Int] -> [[Int]]
-- toStack xs
--  | all (0==) xs = [xs]
--  | otherwise    = xs : toStack (diff xs)
toStack xs = if all (0==) xs then [xs] else xs : toStack (diff xs)

genForward :: [[Int]] -> Int
genForward = sum . map last

genBackward :: [[Int]] -> Int
genBackward = foldr ((-) . head) 0

part1 :: IO Int
part1 = sum . map (genForward . toStack . map read . words) . lines <$> readFile "Day_9/input.txt"

part2 = sum . map (genBackward . toStack . map read . words) . lines <$> readFile "Day_9/input.txt"