import Data.Array
import Control.Arrow
import Data.Functor
import Data.Set (Set)
import qualified Data.Set as S

import Debug.Trace
import Data.Maybe
import Control.Monad

debug x = traceShow x x

data Dir = U | R | D | L deriving (Show, Eq, Ord)
type Pos = (Int,Int) -- y x
type State = (Pos,Dir)
type Board = Array Pos Char

m :: Dir -> Pos -> Pos
m U = first (subtract 1)
m D = first (+1)
m L = fmap (subtract 1)
m R = fmap (+1)

move :: Board -> State -> Maybe State
move b (p,d) = guard (inRange (bounds b) p') $> (p',d)
  where p' = m d p

step :: Board -> State -> [State]
step b s@(p,d) = mapMaybe moveTo $ go (b ! p) d
  where
    moveTo = move b . (s $>)

    go '|' L = [U,D]
    go '|' R = [U,D]

    go '-' U = [L,R]
    go '-' D = [L,R]

    go '/' R = [U]
    go '/' U = [R]
    go '/' D = [L]
    go '/' L = [D]

    go '\\' D = [R]
    go '\\' R = [D]
    go '\\' U = [L]
    go '\\' L = [U]

    go _ d = [d]

dfs :: Board -> Set State -> State -> Set State
dfs b seen bs
 | bs `S.member` seen = seen
 | otherwise = foldr (flip $ dfs b) (S.insert bs seen) $ step b bs

dfsCoords :: Board -> State -> Set Pos
dfsCoords b = S.map fst . dfs b S.empty

makeArray :: String -> Board
makeArray txt = listArray ends $ concat ls
  where
    ls = lines txt
    w = length $ head ls
    h = length ls
    ends = ((0,0),(h-1,w-1))

tabulate :: Ix t => (t, t) -> (t -> e) -> Array t e
tabulate idxs f = array idxs (map (\i->(i,f i)) (range idxs))

printB :: Board -> IO ()
printB b = mapM_ printLine [0..h]
  where
    (_,(h,w)) = bounds b
    printLine y = putStrLn $ map ((b !) . (y,)) [0..w]

part1 :: IO Int
part1 = length . (`dfsCoords`((0,0),R)) . makeArray <$> readFile "Day_16/input.txt"


{-

Map of State -> coords reachable from said state
Utilise max laziness and define an array??

-}

-- part1 :: IO Int
-- part1 = do
--     txt <- readFile "Day_16/test.txt"
--     let board = makeArray txt
--         seen = dfsCoords board ((0,0),R)
--         other = tabulate (bounds board) $ \i -> if i `S.member` seen
--                                                 then '#' else board ! i
--     printB board
--     putStrLn ""
--     printB other
--     return $ length seen