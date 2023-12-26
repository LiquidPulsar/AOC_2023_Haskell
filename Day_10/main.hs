import Debug.Trace
import Data.Array
import Data.Maybe

data Dir = U | R | D | L deriving (Show, Eq)
type Pos = (Int,Int) -- y x
type State = (Pos,Dir)
type Board = Array Pos Char

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.).(.)
infixr 8 ... 

move :: Pos -> Dir -> State
move (y,x) U = ((y-1,x), U)
move (y,x) R = ((y,x+1), R)
move (y,x) D = ((y+1,x), D)
move (y,x) L = ((y,x-1), L)

nextDir :: Char -> Dir -> Maybe Dir
nextDir 'F' U = Just R
nextDir 'F' L = Just D
nextDir '7' R = Just D
nextDir '7' U = Just L
nextDir 'L' D = Just R
nextDir 'L' L = Just U
nextDir 'J' D = Just L
nextDir 'J' R = Just U
nextDir '-' R = Just R
nextDir '-' L = Just L
nextDir '|' U = Just U
nextDir '|' D = Just D
nextDir _ _ = Nothing

step :: Pos -> Char -> Dir -> State
step p = move p . fromJust ... nextDir

stepArr :: Board -> State -> State
stepArr arr (p,d) = step p (arr ! p) d

canStep :: Board -> State -> Bool
canStep arr (p,d) = isJust $ nextDir (arr ! p) d

debug :: Show b => b -> b
debug x = traceShow x x

makeArray :: [String] -> (Board,Pos,State)
makeArray board = (arr,start,dir)
  where
    w = length . head $ board
    h = length board
    ends = ((0,0),(h-1,w-1))
    arr = listArray ends $ concat board
    start = head . filter ((=='S') . (arr !)) . range $ ends
    dir = head . filter (canStep arr) . map (move start) $ [U,D,L,R]

loop :: Board -> (Pos,Dir) -> [Pos]
loop arr pd@(p,_) = p : loop arr (stepArr arr pd)

part1 :: IO Int
part1 = do
    (arr, start, direction) <- makeArray . lines <$> readFile "Day_10/test.txt"
    return $ (`div`2) . (1+) . length $ takeWhile (start/=) $ loop arr direction

part2 :: IO Int
part2 = do
    (arr, start, direction) <- makeArray . lines <$> readFile "Day_10/input.txt"
    let path = start : takeWhile (start/=) (loop arr direction)
        area = (`div`2) . abs . sum $ 
            zipWith (\(x1,y1) (x2,y2) -> x1*y2 - x2*y1) path (tail path ++ [start])
    return $ area + 1 - (length path `div` 2)