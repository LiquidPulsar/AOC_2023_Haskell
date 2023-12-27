import Data.Array

type Pos = (Int,Int) -- y x
type Board = Array Pos

prefixList :: [Int] -> [Int]
prefixList = scanl1 (+)

prefixArr :: Int -> [Int] -> Array Int Int
prefixArr i = listArray (0,i-1) . prefixList

column :: Board Char -> Int -> Int -> [Char]
column arr h x = [arr ! (y,x) | y <- [0..h-1]]

row :: Board Char -> Int -> Int -> [Char]
row arr w y = [arr ! (y,x) | x <- [0..w-1]]

empty :: [Char] -> Bool
empty = all ('.'==)

makeArray :: Int -> [String] -> Int
makeArray scale board = dists galaxies 0
  where
    w = length . head $ board
    h = length board
    arr = listArray ((0,0),(h-1,w-1)) $ concat board
    galaxies = [(y,x) | x <- [0..w-1], y <- [0..h-1], arr ! (y,x) == '#']
    xPrefs = prefixArr h $ [(scale -1) * fromEnum (empty $ column arr h x) | x <- [0..w-1]]
    yPrefs = prefixArr w $ [(scale -1) * fromEnum (empty $ row arr w y) | y <- [0..h-1]]

    diff :: Array Int Int -> Int -> Int -> Int
    diff prefs a b
      | a > b     = diff prefs b a
      | otherwise = b - a + prefs ! b - prefs ! a

    dist :: Pos -> Pos -> Int
    dist (y1,x1) (y2,x2) = diff xPrefs x1 x2 + diff yPrefs y1 y2
    
    dists [] n = n
    dists (h:t) n = dists t $ n + sum (map (dist h) t)

part1 :: IO Int
part1 = makeArray 2 . lines <$> readFile "Day_11/input.txt"

part2 :: IO Int
part2 = makeArray 1_000_000 . lines <$> readFile "Day_11/input.txt"