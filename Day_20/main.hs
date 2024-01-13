{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
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

import Debug.Trace
-- debug = join traceShow


type Signal  = Bool -- True = High
type ModName = String
type Ins     = Map ModName Signal
type Outs    = [ModName]
data Module  = Flip Signal Outs | Conj Ins Outs | Broad Outs deriving Show
type Modules = Map ModName Module
type Counts = (Int, Int)

type St = (Signal, ModName, ModName)
type ModState = State (Modules, Seq St)

popLeft :: ModState (Maybe St)
popLeft = get >>= liftM2 (>>) (put . second (Se.drop 1)) (return . (Se.!? 0) . snd)

pushRight :: St -> ModState ()
pushRight = modify . second . flip (Se.|>)

extendRight :: [St] -> ModState ()
extendRight = foldr ((>>) . pushRight) $ pure ()

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
  process' (sig, m, from) = maybe next (go sig from m) . (M.!? m) =<< getMods
    where
      go :: Signal -> ModName -> ModName -> Module -> ModState Counts
      go sig from name me = updateMods name nmod >> extendRight (map (nsig,,name) nmes) >> next
        where (nmod, nsig, nmes) = send me sig from
      next = process $ update is sig
      -- !_ = trace (from ++ " -" ++ sName sig ++ "-> " ++ m) 0
  update (a,b) False = (a+1,b)
  update (a,b) True = (a,b+1)

loopProg :: Int -> Counts -> Modules -> Counts
loopProg 0 cs _ = cs
loopProg i cs mods = loopProg (i-1) cs' mods'
  where (cs',(mods',Se.Empty)) = runState (process cs) (mods, Se.singleton (False, "broadcaster", "button"))

sName False = "low"
sName True  = "high"

send :: Module -> Signal -> ModName -> (Module,Signal,[ModName])
send mod@(Flip _ _) True _ = (mod, True, [])
send (Flip saved os) False _ = (Flip (not saved) os, not saved, os)
send (Conj ins outs) sig name = (Conj ins' outs, not . and $ M.elems ins', outs)
  where ins' = M.insert name sig ins
send mod@(Broad os) sig _ = (mod,sig,os)

parseLine :: String -> Modules -> Modules
parseLine line mods = foldr (\target acc -> updateMap acc name target) (M.insert name mod mods) outs
  where
    [l,r] = T.splitOn " -> " $ pack line
    outs = map unpack $ T.splitOn ", " r
    (name, mod) = parse $ unpack l

    parse :: ModName -> (ModName, Module)
    parse l@('b':_) = (l, Broad outs)
    parse ('%':l) = (l, Flip False outs)
    parse ('&':l) = (l, Conj M.empty outs)
    parse [] = error "Empty line"
    parse (c:_) = error $ "Invalid first char" ++ [c]

    updateMap :: Modules -> ModName -> ModName -> Modules
    updateMap mods src target = case mods M.!? target of
      Just (Conj is os) -> M.insert target (Conj (M.insert src False is) os) mods
      _ -> mods


part1 :: IO Int
part1 = uncurry (*) . loopProg 1000 (0,0) . foldr parseLine M.empty . lines <$> readFile "Day_20/input.txt"