
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char
import Control.Arrow

hash :: Text -> Int
hash = T.foldl (\x c -> ((x + ord c) * 17) `mod` 256) 0

bits :: IO [Text]
bits = T.split (','==) . T.pack <$> readFile "Day_15/input.txt"

part1 :: IO Int
part1 = sum . map hash <$> bits

{-

Map and Tree
Map is K : (V,age)

Tree of keys sorted by age, nodes have V too

When iterating we get insertion order
When updating we go to the tree remove the old entry and add new one in
(probably changing tree structure)

-}


data Tree v = Empty
            | Node (Tree v) v Int (Tree v)
            deriving Foldable

showTree :: Show v => String -> Tree v -> String
showTree s Empty = s <> "Empty"
showTree s (Node l v i r) = concat [
  s, show v, " ", show i, "\n",
  showTree (' ':s) l, "\n",
  showTree (' ':s) r ]

instance Show v => Show (Tree v) where
  show = showTree ""

leaf :: v -> Int -> Tree v
leaf v i = Node Empty v i Empty

insertTree :: v -> Int -> Tree v -> Tree v
insertTree v i Empty = leaf v i
insertTree v i (Node l v' i' r)
 | i < i'    = Node (insertTree v i l) v' i' r
 | i == i'   = error $ "No node exists with age " ++ show i
 | otherwise = Node l v' i' (insertTree v i r)

splitLeftMost :: Tree v -> (v, Int, Tree v)
splitLeftMost Empty = error "Splitting off empty tree!"
splitLeftMost (Node Empty v i t) = (v, i, t)
splitLeftMost (Node l v i r) = (v', i', Node t' v i r)
  where (v',i',t') = splitLeftMost l

splitRightMost :: Tree v -> (v, Int, Tree v)
splitRightMost Empty = error "Splitting off empty tree!"
splitRightMost (Node t v i Empty) = (v, i, t)
splitRightMost (Node l v i r) = (v', i', Node l v i t')
  where (v',i',t') = splitRightMost r

removeTree :: Int -> Tree v -> Tree v
removeTree i Empty = error $ "No node exists with age " ++ show i
removeTree i (Node l v' i' r)
 | i < i'    = Node (removeTree i l) v' i' r
 | i == i' && not (null r) = let (v'',i'',t'') = splitLeftMost r in Node l v'' i'' t''
 | i == i' && not (null l) = let (v'',i'',t'') = splitRightMost r in Node t'' v'' i'' r
 | i == i'                 = Empty
 | otherwise = Node l v' i' (removeTree i r)

-- Update known contained entry
updateTree :: v -> Int -> Int -> Tree v -> Tree v
updateTree v old new = insertTree v new . removeTree old

-- Doesn't update age of touched node. Entry known contained
modifyTree :: v -> Int -> Tree v -> Tree v
modifyTree v i Empty = error $ "No node exists with age " ++ show i
modifyTree v i (Node l v' i' r)
 | i < i'    = Node (modifyTree v i l) v' i' r
 | i == i'   = Node l v i' r
 | otherwise = Node l v' i' (modifyTree v i r)

makeMap :: Ord a => [a] -> [b] -> OMap a b
makeMap as bs = foldr ($) empty $ zipWith insert (reverse as) bs

data OMap k v where
  OMap :: Ord k => {dct :: Map k (v, Int), ages :: Tree (k, v), next :: Int} 
                -> OMap k v

instance (Show k, Show v) => Show (OMap k v) where
  show :: (Show k, Show v) => OMap k v -> String
  show = show . ages

empty :: Ord k => OMap k v
empty = OMap {
  dct = M.empty,
  ages = Empty,
  next = 0
}

-- LRU MODE \/
insertLRU :: k -> v -> OMap k v -> OMap k v
insertLRU k v (OMap d a n) = OMap {
  dct = M.insert k (v,n) d,
  ages = case d M.!? k of
           Nothing -> insertTree (k,v) n a
           Just (_,n') -> updateTree (k,v) n' n a,
  next = n + 1
}

insert :: k -> v -> OMap k v -> OMap k v
insert k v om@(OMap d a n) = case d M.!? k of
  Nothing -> OMap {
    dct = M.insert k (v,n) d,
    ages = insertTree (k,v) n a,
    next = n + 1
  }
  Just (_,n') -> OMap {
    dct = M.adjust (first $ const v) k d,
    ages = modifyTree (k,v) n' a,
    next = n + 1
  }

-- No-op if key not in map
pop :: k -> OMap k v -> OMap k v
pop k om@(OMap d a n) = case d M.!? k of
  Nothing -> om
  Just (_,n') -> om {
    dct = M.delete k d,
    ages = removeTree n' a
  }

assocsOm :: OMap k v -> [(k,v)]
assocsOm = foldr (:) [] . ages

-- All this work to add a single letter to 
-- the front of "Map" in that type def lmao

type Box = OMap Text Int
type Boxes = Map Int Box

emptyBoxes :: Boxes
emptyBoxes = M.fromAscList $ zip [0..255] $ replicate 256 empty

part2 :: IO Int
part2 = do
  bs <- bits
  let res = foldr parse emptyBoxes $ reverse bs

      parse :: Text -> Boxes -> Boxes
      parse t !boxes
       | T.last t == '-' = M.adjust (pop l1) h1 boxes
       | otherwise = M.adjust (insert l2 (read [T.last t])) h2 boxes
       where
        l1 = T.init t
        h1 = hash l1
        l2 = T.dropEnd 2 t
        h2 = hash l2

      scoreBox :: Box -> Int
      scoreBox = sum . zipWith (*) [1..] . map snd . assocsOm
  -- mapM_ print $ filter (not . null . dct . snd) $ M.assocs res
  return . sum . zipWith (*) [1..] . map scoreBox $ M.elems res