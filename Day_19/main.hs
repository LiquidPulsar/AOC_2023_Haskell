import Data.Functor
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Char (isDigit)
import Control.Arrow ( Arrow(first) )

import Data.Maybe
import Data.Foldable (asum)

data Xmas = Xmas{x,m,a,s::Int} deriving (Read,Show)
type XmasRange = (Xmas,Xmas)
data Rule = Rule Char String Int String deriving (Eq,Ord,Show)
type Workflow = ([Rule],String)
type Workflows = Map String Workflow

rating :: Xmas -> Int
rating (Xmas x m a s) = x + m + a + s

parseRule :: String -> Rule
parseRule [] = error "Empty rule"
parseRule (l:r) = Rule l f (read i) o
  where
    (f,r') = break isDigit r
    (i,_:o) = span isDigit r'

parseLine :: String -> (String, Workflow)
parseLine s = (name, bits)
  where
    (name,_:rest) = break ('{'==) s
    bits = parseBits . T.split (','==) . T.pack $ init rest
    
    parseBits :: [Text] -> Workflow
    parseBits [] = error "Empty bits"
    parseBits [x] = pure $ T.unpack x
    parseBits (h:t) = first (parseRule (T.unpack h) :) $ parseBits t

parseFile :: ([String],[String]) -> ((Map String Workflow, Workflow),[Xmas])
parseFile (a,_:b) = (ap (,) (M.! "in") $ M.fromList $ map parseLine a, map parseXmas b)
parseFile _ = error "Invalid input"

parseXmas :: String -> Xmas
parseXmas s = read $ "Xmas" ++ s

getC :: Char -> Xmas -> Int
getC 'x' = x
getC 'm' = m
getC 'a' = a
getC 's' = s
getC _ = error "Invalid char"

getF :: String -> (Int -> Int -> Bool)
getF "<" = (<)
getF "<=" = (<=)
getF ">" = (>)
getF ">=" = (>=)
getF _ = error "Invalid string"

passes :: Rule -> Xmas -> Maybe String
passes (Rule c f i o) = ($> o) . guard . (flip $ getF f) i . getC c

evalWorkflow :: (Workflows, Workflow) -> Xmas -> Bool
evalWorkflow (wfs,(rs,o)) xm = proceed . fromMaybe o $ msum (map (`passes`xm) rs)
  where
    proceed :: String -> Bool
    proceed "A" = True
    proceed "R" = False
    proceed name = evalWorkflow (wfs,wfs M.! name) xm

parsedData :: IO ((Map String Workflow, Workflow), [Xmas])
parsedData = parseFile . break null . lines <$> readFile "Day_19/input.txt"

passR :: Rule -> XmasRange -> Maybe (String, XmasRange, XmasRange)
passR (Rule c f i o) lh@(lo,hi) = guard ok $> (o,lh',rest)
  where
    ok = getF f cl i || getF f ch i
    cl = getC c lo
    ch = getC c hi
    ((x,y),(z,a)) = segment cl ch f i
    lh' = makeXmasR c x y lh
    rest = makeXmasR c z a lh

segment :: Int -> Int -> String -> Int -> ((Int,Int),(Int,Int))
segment lo hi "<" mid  = ((lo,mid-1),(mid,hi))
segment lo hi "<=" mid = ((lo,mid),(mid+1,hi))
segment lo hi ">" mid  = ((mid+1,hi),(lo,mid))
segment lo hi ">=" mid = ((mid,hi),(lo,mid-1))
segment _ _ _ _        = error "Invalid string"

makeXmasR :: Char -> Int -> Int -> XmasRange -> XmasRange
makeXmasR 'x' l h (a,b) = (a{x=l},b{x=h})
makeXmasR 'm' l h (a,b) = (a{m=l},b{m=h})
makeXmasR 'a' l h (a,b) = (a{a=l},b{a=h})
makeXmasR 's' l h (a,b) = (a{s=l},b{s=h})
makeXmasR _ _ _ _       = error "Invalid char"

evalWorkflow2 :: (Workflows, Workflow) -> XmasRange -> Int
evalWorkflow2 (wfs,(rs,o)) = go rs
  where
    go :: [Rule] -> XmasRange -> Int
    go []    xr = proceed o xr
    go (h:t) xr = maybe (go t xr) pgo (passR h xr)
      where pgo (name, xr', rest) = proceed name xr' + go t rest

    proceed :: String -> XmasRange -> Int
    proceed "A" (Xmas x m a s, Xmas x' m' a' s') 
                    = (x' - x + 1) * (m' - m + 1) * (a' - a + 1) * (s' - s + 1)
    proceed "R" _   = 0
    proceed name xr = evalWorkflow2 (wfs,wfs M.! name) xr

fullR :: XmasRange
fullR = (Xmas 1 1 1 1, Xmas 4000 4000 4000 4000)

part1 :: IO Int
part1 = sum . map rating . uncurry (filter . evalWorkflow) <$> parsedData

part2 :: IO Int
part2 = (`evalWorkflow2` fullR) . fst <$> parsedData