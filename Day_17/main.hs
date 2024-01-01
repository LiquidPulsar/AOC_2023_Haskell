{-# LANGUAGE ViewPatterns #-}

import Data.Array
import Control.Arrow
import Control.Monad
import Data.Functor
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S

data Dir = U | R | D | L deriving (Show, Eq, Ord, Ix, Enum)
type Pos = (Int,Int) -- y x
type State = (Pos,Dir,Int)
type Board = Array Pos Int
type Ss = [State]

nxt :: Dir -> Dir
nxt L = U
nxt x = succ x

prv :: Dir -> Dir
prv U = L
prv x = pred x

data MoveList = ML {
    curr :: Ss,
    p1 :: Ss,
    p2 :: Ss,
    p3 :: Ss,
    p4 :: Ss,
    p5 :: Ss,
    p6 :: Ss,
    p7 :: Ss,
    p8 :: Ss,
    p9 :: Ss
} deriving Show

empty :: MoveList
empty = ML [] [] [] [] [] [] [] [] [] []

add :: MoveList -> Int -> State -> MoveList
add ml 0 st = ml{curr = st:curr ml}
add ml 1 st = ml{p1 = st:p1 ml}
add ml 2 st = ml{p2 = st:p2 ml}
add ml 3 st = ml{p3 = st:p3 ml}
add ml 4 st = ml{p4 = st:p4 ml}
add ml 5 st = ml{p5 = st:p5 ml}
add ml 6 st = ml{p6 = st:p6 ml}
add ml 7 st = ml{p7 = st:p7 ml}
add ml 8 st = ml{p8 = st:p8 ml}
add ml 9 st = ml{p9 = st:p9 ml}

shift :: MoveList -> MoveList
shift (ML _ a b c d e f g h i) = ML a b c d e f g h i []

makeArray :: String -> Board
makeArray txt = listArray ends . map (read . pure) $ concat ls
  where
    ls = lines txt
    w = length $ head ls
    h = length ls
    ends = ((0,0),(h-1,w-1))

m :: Dir -> Pos -> Pos
m U = first (subtract 1)
m D = first (+1)
m L = fmap (subtract 1)
m R = fmap (+1)

move :: Int -> Board -> State -> Maybe State
move hi b (p,d,i) = guard valid $> (p',d,i+1)
  where
    p' = m d p
    valid = inRange (bounds b) p' && i < hi

turn :: Int -> Int -> Board -> State -> Maybe State
turn lo hi b (p,d,i) = guard (i>=lo) >> move hi b (p,d,0) -- guard (i>=3) >> 

turns :: Int -> Int -> Board -> State -> [Maybe State]
turns lo hi b (p,d,i) = map (turn lo hi b . (p,,i)) [nxt d, prv d]

-- Every cell has values 1-9: Store 10 lists
search :: Int -> Int -> Board -> Set State -> Int -> MoveList -> Int
search lo hi b = go
  where
    go _ i (ML [] [] [] [] [] [] [] [] [] []) = error "AAAA"
    go c i ml@(curr -> []) = go c (i + 1) $ shift ml
    go c i ml@(curr -> (h@(p,d,l):t))
     | S.member h c        = go c i ml{curr=t}
     | p == snd (bounds b) = i + b ! p
     | otherwise           = go (S.insert h c) i ml'
     where
        moves = catMaybes $ move hi b h : turns lo hi b h
        ml' = foldr (flip (`add`(b ! p))) ml{curr=t} moves

start :: MoveList
start = empty{curr=[((0,1),R,1),((1,0),D,1)]}

gameBoard :: IO Board
gameBoard = makeArray <$> readFile "Day_17/input.txt"

part1 :: IO Int
part1 = (\b -> search 0 3 b S.empty 0 start) <$> gameBoard

part2 :: IO Int
part2 = (\b -> search 4 10 b S.empty 0 start) <$> gameBoard
