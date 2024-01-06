import Data.Functor
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Char (isDigit)
import Control.Arrow ( Arrow(first) )

import Debug.Trace
import Data.Maybe
import Control.Applicative

debug x = traceShow x x

data Xmas = Xmas{x,m,a,s::Int} deriving (Read,Show)
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

evalWorkflow :: Workflows -> Workflow -> Xmas -> Bool
evalWorkflow wfs (rs,o) xm = go rs
  where
    go :: [Rule] -> Bool
    go [] = proceed o
    go (h:t) = case passes h xm of
      Nothing -> go t
      Just s -> proceed s

    proceed :: String -> Bool
    proceed "A" = True
    proceed "R" = False
    proceed name = evalWorkflow wfs (wfs M.! name) xm

trackWorkflow :: Workflows -> Workflow -> Xmas -> [String]
trackWorkflow wfs (rs,o) xm = go rs
  where
    go :: [Rule] -> [String]
    go [] = proceed o
    go (h:t) = case passes h xm of
      Nothing -> go t
      Just s -> proceed s

    proceed :: String -> [String]
    proceed "A" = ["A"]
    proceed "R" = ["R"]
    proceed name = name : trackWorkflow wfs (wfs M.! name) xm

part1 :: IO Int
part1 = do
    (a,_:b) <- break null . lines <$> readFile "Day_19/input.txt"

    let wfs = M.fromList $ map parseLine a
        xms = map parseXmas b
    -- print wfs
    -- print $ map (trackWorkflow wfs (wfs M.! "in")) xms

    return . sum . map rating . filter (evalWorkflow wfs (wfs M.! "in")) $ xms
    -- return . filter (evalWorkflow wfs (wfs M.! "in")) $ xms
