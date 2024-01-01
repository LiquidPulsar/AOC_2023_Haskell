import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow
import Data.Tuple
import Data.Char

data Dir = R | D | L | U deriving (Show, Eq, Ord, Enum, Read)
type Pos = (Int,Int) -- y x
type Instr = (Dir,Int)

parseLine :: String -> Instr
parseLine s = (read a, read b)
  where [a,b,_] = map T.unpack . T.split (' '==) $ T.pack s

readHex :: String -> Int
readHex = foldl (\acc x -> acc*16 + hexChar x) 0
  where
    hexChar :: Char -> Int
    hexChar c
     | c <= '9' = ord c - ord '0'
     | otherwise = 10 + ord c - ord 'a'

parseLine2 :: String -> Instr
parseLine2 = ((toEnum . read . take 1) *** readHex) . swap . splitAt 5 . drop 2 . T.unpack . last . T.split (' '==) . T.pack

{-# INLINE m #-}
m :: Dir -> (Int -> (Pos -> Pos))
m U = first . subtract
m D = first . (+)
m L = fmap . subtract
m R = fmap . (+)

mkShoelace :: Pos -> [Instr] -> ([Pos],Int)
mkShoelace _ [] = pure 0
mkShoelace p ((d,i):t) = (p:) *** (i+) $ mkShoelace (m d i p) t

area :: [Pos] -> Int
area xs = (`div`2) . abs . sum $ zipWith term xs (last xs : init xs)
  where term (y1,x1) (y2,x2) = y1*x2 - y2*x1

part1 :: IO Int
part1 = (\(lace,tot) -> area lace + (tot `div` 2) + 1) . mkShoelace (0,0) . map parseLine . lines <$> readFile "Day_18/input.txt"

part2 :: IO Int
part2 = (\(lace,tot) -> area lace + (tot `div` 2) + 1) . mkShoelace (0,0) . map parseLine2 . lines <$> readFile "Day_18/input.txt"
