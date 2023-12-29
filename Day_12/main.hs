import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Array
import Control.Monad
import Data.Functor
import Data.Maybe

type Pat = String

parseLine :: [String] -> (Pat,[Int])
parseLine [pat,nums] = (pat, read $ '[':nums++"]")

parseLine2 :: [String] -> (Pat,[Int])
parseLine2 str = let (a,b) = parseLine str 
                 in (intercalate "?" $ replicate 5 a,
                     concat $ replicate 5 b)

canFit :: Int -> Pat -> Bool
canFit num str = num <= l
                && notElem '.' a
                && (num == l || head b /= '#')
                where
                    l = length str
                    (a,b) = splitAt num str

naive :: Pat -> [Int] -> Int
naive ps [] = fromEnum $ notElem '#' ps
naive [] _ = 0
naive ps@('#':_) (n:ns) = if canFit n ps then naive (drop (n+1) ps) ns else 0
naive ('.':rest) ns = naive rest ns
naive ('?':rest) ns = naive ('#':rest) ns + naive ('.':rest) ns

tabulate :: Ix t => (t, t) -> (t -> e) -> Array t e
tabulate idxs f = array idxs (map (\i->(i,f i)) (range idxs))

solve :: Pat -> [Int] -> Int
solve pat is = table ! (pl,il)
  where
    parr  = T.pack pat
    pl = T.length parr

    il = length is
    iarr  = listArray (0,pl-1) is
    table = tabulate ((0,0),(pl,il)) $ uncurry solve'

    -- *After* this position (from the end), there are no #s
    pos = fromMaybe pl . fst $ foldr go (Nothing,0) pat
      where
        {-# INLINE go #-}
        go :: Char -> (Maybe Int, Int) -> (Maybe Int, Int)
        go c (Nothing,n) = (guard (c=='#') $> n,n+1)
        go _ acc = fmap (+1) acc

    solve' :: Int -> Int -> Int
    solve' pi 0 = fromEnum $ pos >= pi
    solve' 0 _ = 0
    solve' pi ni 
     = let
        n = iarr ! (il - ni)
        hash_res = if canFitArr n pi
                    then table ! (max (pi - n - 1) 0, ni - 1)
                    else 0
        dot_res = table ! (pi-1,ni)
      in case parr `T.index` (pl - pi) of
        '#' -> hash_res
        '.' -> dot_res
        '?' -> hash_res + dot_res

    {-# INLINE canFitArr #-}
    canFitArr :: Int -> Int -> Bool
    canFitArr n pi = n <= pi
                   && not (T.elem '.' a )
                   && (n == pi || T.head b /= '#')
      where
        (a,b) = T.splitAt n $ T.takeEnd pi parr

part1 :: IO Int -- Naive is faster on small input lol
part1 = sum . map (uncurry naive . parseLine . words) . lines <$> readFile "Day_12/input.txt"

part2 :: IO Int
part2 = sum . map (uncurry solve . parseLine2 . words) . lines <$> readFile "Day_12/input.txt"