{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Map (Map, (!))
import qualified Data.Map as M
import Control.Monad
import Control.Arrow

import Control.Monad.Trans.State

import Data.Sequence (Seq)
import qualified Data.Sequence as Se


type Signal  = Bool -- True = High
type ModName = String
type Ins     = Map ModName Signal
type Outs    = [ModName]
data Module  = Flip Signal Outs | Conj Signal Ins Outs | Broad Outs deriving Show
type Modules = Map ModName Module
type Counts = (Int, Int)

type St = (Signal, ModName, ModName)
type ModState = State (Modules, Seq St)

outs :: Module -> Outs
outs (Flip _ o) = o
outs (Conj _ _ o) = o
outs (Broad o)  = o

popLeft :: ModState (Maybe St)
popLeft = get >>= liftM2 (>>) (put . second (Se.drop 1)) (return . (Se.!? 0) . snd)

pushRight :: St -> ModState ()
pushRight = modify . second . flip (Se.|>)

extendRight :: [St] -> ModState ()
extendRight = mapM_ pushRight

getSeq :: ModState (Seq St)
getSeq = snd <$> get

getMods :: ModState Modules
getMods = fst <$> get

updateMods :: ModName -> Module -> ModState ()
updateMods n = modify . first . M.insert n

process :: Counts -> ModState Counts
process is = popLeft >>= maybe (pure is) process'
  where
    process' :: (Signal, ModName, ModName) -> ModState Counts
    process' (sig, name, from) = getMods >>= maybe next go . (M.!? name)
      where
        go :: Module -> ModState Counts
        go me = updateMods name nmod >> extendRight (map (nsig,,name) nmes) >> next
          where (nmod, nsig, nmes) = send me sig from
        next = process $ update is sig
    update (a,b) False = (a+1,b)
    update (a,b) True = (a,b+1)

runSignal :: (Counts, Modules) -> (Counts, Modules)
runSignal (cs,mods) = second fst $ runState (process cs) (mods, Se.singleton (False, "broadcaster", "button"))

loopProg :: Int -> Counts -> Modules -> Counts
loopProg i cs mods = fst $ iterate runSignal (cs,mods) !! i

send :: Module -> Signal -> ModName -> (Module,Signal,[ModName])
send mod@(Flip _ _) True _ = (mod, True, [])
send (Flip saved os) False _ = (Flip (not saved) os, not saved, os)
-- AND the signal with the old one so we can see if any falses were found
send (Conj old ins outs) sig name = (Conj (sig && old) ins' outs, not . and $ M.elems ins', outs)
  where ins' = M.insert name sig ins
send mod@(Broad os) sig _ = (mod,sig,os)

parseLine :: String -> Modules -> Modules
parseLine line mods = foldr (updateMap name) (insertMod mod) outs
  where
    [l,r] = T.splitOn " -> " $ pack line
    outs = map unpack $ T.splitOn ", " r
    (name, mod) = parse $ unpack l

    {-
      Dealing with dummy from below.
      Only care if we insert a conj over a dummy, otherwise a normal insert
    -}
    insertMod :: Module -> Modules
    insertMod (Conj _ _ os) = case mods M.!? name of
      Just (Conj _ is []) -> M.insert name (Conj True is os) mods
      _ -> M.insert name mod mods
    insertMod _ = M.insert name mod mods

    parse :: ModName -> (ModName, Module)
    parse l@('b':_) = (l, Broad outs)
    parse ('%':l) = (l, Flip False outs)
    parse ('&':l) = (l, Conj True M.empty outs)
    parse [] = error "Empty line"
    parse (c:_) = error $ "Invalid first char" ++ [c]

    {-
      If the target doesn't exist yet, we make a dummy Conj. 
      This will only ever manually get replaced once: 
        - With a different type in which case we don't care
        - With a Conj in which case it'll take the in list from the dummy
    -}
    updateMap :: ModName -> ModName -> Modules -> Modules
    updateMap src target mods = case mods M.!? target of
      Just (Conj True is os) -> M.insert target (Conj True (M.insert src False is) os) mods
      Nothing -> M.insert target (Conj True (M.singleton src False) []) mods
      _ -> mods


part1 :: IO Int
part1 = uncurry (*) . loopProg 1000 (0,0) . foldr parseLine M.empty . lines <$> readFile "Day_20/input.txt"

findTime :: Modules -> Int
findTime mods = go mods M.empty 1
  where
    counters = M.keys $ M.filter (elem "vr" . outs) mods -- vr alone points to rx directly
    n = length counters
    go ms cs i
      | M.size cs == n = foldr1 lcm $ M.elems cs
      | otherwise      = go (resetConjs ms') cs' (i+1)
      where
        (_,ms') = runSignal ((0,0),ms)
        cs' = foldr (`M.insert`i) cs . M.keys . M.filterWithKey (\k v -> k `elem` counters && isOn v) $ ms'
    
    isOn (Conj s _ _) = not s
    isOn _ = False

    resetConjs :: Modules -> Modules
    resetConjs = M.map f
      where
        f (Conj _  is os) = Conj True is os
        f x = x

part2 :: IO Int
part2 = findTime . foldr parseLine M.empty . lines <$> readFile "Day_20/input.txt"