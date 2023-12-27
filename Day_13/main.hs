import Data.List
import Data.Maybe
import Control.Applicative
import GHC.Bits

isPower2 :: (Bits i, Integral i) => i -> Bool
isPower2 0 = False
isPower2 n = n .&. (n-1) == 0

diffByOneBit :: Int -> Int -> Bool
diffByOneBit a b = isPower2 $ a `xor` b

strictEq :: [Int] -> [Int] -> Bool
strictEq (x:xs) (y:ys) = x==y && strictEq xs ys
strictEq _ _ = True

foldIntsWith :: ([Int] -> [Int] -> Bool) -> [Int] -> Maybe Int
foldIntsWith _ [] = Nothing
foldIntsWith eq (i:is) = listToMaybe $ go is [i] 1
  where
    go :: [Int] -> [Int] -> Int -> [Int]
    go [] _  _ = []
    go x@(h:t) y i = [i | eq x y] ++ go t (h:y) (i+1)

strictFold :: [Int] -> Maybe Int
strictFold = foldIntsWith strictEq

laxFold :: [Int] -> Maybe Int
laxFold = foldIntsWith offByOne
  where
    offByOne :: [Int] -> [Int] -> Bool
    offByOne (x:xs) (y:ys)
     | x == y        = offByOne xs ys
     | diffByOneBit x y = strictEq xs ys 
    offByOne _ _     = False

encode :: [Char] -> Int
encode [] = 0
encode (h:t) = (encode t * 2) + fromEnum (h == '#')

parse :: [String] -> [[String]]
parse = map reverse . go []
  where
    go x [] = x
    go x ("":t) = go ([]:x) t
    go (p:ps) (h:t) = go ((h:p):ps) t
    go [] (h:t) = go [[h]] t

solve :: ([Int] -> Maybe Int) -> [String] -> Int
solve f lines = fromJust $ row <|> col
  where
    row = fmap (*100) . f $ map encode lines
    col = f . map encode $ transpose lines

part1 :: IO Int
part1 = sum . map (solve strictFold) . parse . lines <$> readFile "Day_13/input.txt"

part2 :: IO Int
part2 = sum . map (solve laxFold) . parse . lines <$> readFile "Day_13/input.txt"