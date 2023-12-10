import Data.Char
import System.Process.Internals (translate)
import Data.List

data Range = Int ::: Int deriving (Show, Eq, Ord)
type SMap = [(Range,Int)]

rangeMin :: Range -> Int
rangeMin (lo ::: _) = lo

parseMap :: [String] -> SMap
parseMap = sort . helper
  where
    helper [] = []
    helper (h:t) = (s ::: (s+r), d-s) : helper t
      where [d,s,r] = map read . words $ h

parse1 :: [String] -> ([Int],[SMap])
parse1 (h:t) = (map read . words . dropWhile (not . isDigit) $ h, readMaps t)
  where
    readMaps :: [String] -> [SMap]
    readMaps [] = []
    readMaps (_:_:ls) = parseMap a : readMaps b
      where
        (a,b) = break null ls

lookup1 :: SMap -> Int -> Int
lookup1 [] i = i
lookup1 ((lo ::: hi,d):ms) i
  | i < lo    = i
  | i <= hi   = i + d
  | otherwise = lookup1 ms i

lookupRange :: SMap -> Range -> [Range]
lookupRange [] r = [r]
lookupRange ((lo ::: hi,d):ms) k@(klo ::: khi)
  | khi < lo  = [klo ::: khi]    -- too far left
  | klo >= hi = lookupRange ms k -- too far right
  | khi < hi  = if klo < lo then [klo ::: lo, trans lo khi]
                            else [trans klo khi]
  | khi >= hi = if klo < lo then klo ::: lo : trans lo hi : lookupRange ms (hi ::: khi)
                            else trans klo hi : lookupRange ms (hi ::: khi)
  where
    trans :: Int -> Int -> Range
    trans l h = (l + d) ::: (h + d)

part1 :: IO Int
part1 = do 
    f <- readFile "Day_5/input.txt"
    let (seeds,ms') = parse1 . lines $ f
        ms = reverse ms'
    pure . minimum . map (\x -> foldr lookup1 x ms) $ seeds

part2 :: IO Int
part2 = do 
    f <- readFile "Day_5/input.txt"
    let (seeds,ms') = parse1 . lines $ f
        ms = reverse ms'
        ranges = map (\(a:b:_) -> a ::: (a+b)) . takeWhile (not . null) $ iterate (drop 2) seeds
    pure . minimum . map rangeMin $ foldr (concatMap . lookupRange) ranges ms