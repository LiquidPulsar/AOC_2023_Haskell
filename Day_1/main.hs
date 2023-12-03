import Data.Char
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List
import Data.Function

total :: [Int] -> Int
total x = (10 * head x) + last x

subZero :: Char -> Int
subZero = subtract (ord '0') . ord

doLine :: String -> [Int]
doLine = map subZero . filter isDigit

part1 :: IO Int
part1 = sum . map (total . doLine) . lines <$> readFile "Day_1/input.txt"

doLine2 :: String -> [Int]
doLine2 = mapMaybe parse . liftA2 (:) id tails

digs :: [String]
digs = words "zero one two three four five six seven eight nine"

parse :: String -> Maybe Int
parse [] = Nothing
parse s@(c:_) = ((<|>) `on` listToMaybe) [ subZero c | isDigit c ] [
    i | (i,opt) <- zip [0..] digs,
    opt `isPrefixOf` s]

part2 :: IO Int
part2 = sum . map (total . doLine2) . lines <$> readFile "Day_1/input.txt"