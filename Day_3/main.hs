import Data.Char
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Control.Monad
import Data.List
import Data.Function

type X = Int
type Y = Int
type Coord = (Y,X)

type CMap = M.Map Coord (Int, Coord)
type SMap = M.Map Coord Char

instance {-# OVERLAPPING #-} Show a => Show (Coord,(a,Coord)) where
    show (c,(i,_)) = '(' : show c ++ "->" ++ show i ++ ") "

instance {-# OVERLAPPING #-} Show a => Show (Coord,a) where
    show (c,i) = '(' : show c ++ "->" ++ show i ++ ") "

instance {-# OVERLAPPING #-} Show a => Show (M.Map Coord a) where
    show m = concatMap show (M.assocs m)

buildMap :: Bool -> [String] -> CMap -- Note the coords have inverted Y vals here
buildMap small = snd . foldr parseLine ((0,0),mempty)
  where
    buildLine :: String -> (Coord, CMap) -> (Coord, CMap)
    buildLine [] cm = cm
    buildLine xxs (c@(y,x),m)
      | null int  = buildLine (tail xxs) ((y,x+1),m)
      | small = (c',M.insert c (read int, c) m')
      | otherwise = (c',m'')
      where
        (int,rest) = span isDigit xxs
        l = length int
        (c',m') = buildLine rest ((y,x+l),m)
        addToMap :: X -> CMap -> CMap
        addToMap xval = M.insert (y,xval) (read int,c)
        m'' = foldr addToMap m' [x..x+l-1]
    parseLine s cm = let ((y,x),m) = buildLine s cm in ((y+1,0),m)

buildSymbs :: [String] -> SMap -- Note the coords have inverted Y vals here
buildSymbs = snd . foldr parseLine (0,mempty)
  where
    parseLine :: String -> (Y, SMap) -> (Y, SMap)
    parseLine s (y,m) = (y+1, foldr addToMap m $ filter (\(i,c) -> not (isDigit c || c == '.')) $ zip [0..] s)
      where
        addToMap :: (X,Char) -> SMap -> SMap
        addToMap (xval,c) = M.insert (y,xval) c

intSize :: Int -> Int
intSize i
 | i < 0     = error $ "negative num " ++ show i
 | i < 10    = 1
 | otherwise = 1 + intSize (i`div`10)

border :: Int -> Coord -> [Coord]
border l (y,x) = liftM2 (,) [y-1..y+1] [x-1..x+l]

adj :: SMap -> (Int, Coord) -> [Char]
adj s (i,c) = mapMaybe (s M.!?) $ border (intSize i) c

safe :: SMap -> (Int, Coord) -> Bool
safe s = null . adj s

adjNum :: CMap -> Coord -> [Int]
adjNum c ic = map fst . nubBy ((==)`on`snd) . mapMaybe (c M.!?) $ border 1 ic

part1 :: IO Int
part1 = do 
    f <- readFile "Day_3/input.txt"
    let m = buildMap True $ lines f
        s = buildSymbs    $ lines f
    pure $ sum . map fst . filter (not . safe s) $ M.elems m

part2 :: IO Int
part2 = do 
    f <- readFile "Day_3/input.txt"
    let m = buildMap False $ lines f
        s = buildSymbs    $ lines f
    pure . sum . map product . filter ((2==).length) . map (adjNum m) $ M.keys s
