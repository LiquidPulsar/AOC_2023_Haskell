{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Control.Arrow
import Control.Monad.Trans.State

type Rocks = [Text]
type Cache a = (Map Rocks a, [a])
type Cached a = State (Cache a) a

getCacheR :: Rocks -> State (Cache a) (Maybe a)
getCacheR a = gets ((M.!? a) . fst)

getCacheI :: Int -> State (Cache a) a
getCacheI i = gets ((!! i) . reverse . snd)

putCache :: Rocks -> Int -> State (Cache Int) ()
putCache r i = modify $ M.insert r i *** (load r:)

evalWithCache :: Cached a -> a
evalWithCache = (`evalState` mempty)

rotCW :: Rocks -> Rocks
rotCW = map T.reverse . T.transpose

rotCCW :: Rocks -> Rocks
rotCCW = T.transpose . map T.reverse

shiftLeft :: Rocks -> Rocks
shiftLeft = map shiftLine
  where
    shiftLine :: Text -> Text
    shiftLine = T.intercalate "#" . map f . T.split ('#'==)
      where
        f :: Text -> Text
        f txt = T.justifyLeft (T.length txt) '.' $ T.filter ('O'==) txt

loadLine :: Text -> Int
loadLine = snd . T.foldr accum (1,0)
  where
    accum :: Char -> (Int, Int) -> (Int, Int)
    accum 'O' (i,t) = (i+1,t+i)
    accum _ (i,t) = (i+1,t)

load :: Rocks -> Int
load = sum . map loadLine

cyc :: Rocks -> Rocks
cyc = (!!4) . iterate (rotCW . shiftLeft)

printB :: Rocks -> IO ()
printB = mapM_ (putStrLn . T.unpack) . rotCW

getRocks :: IO Rocks
getRocks = rotCCW . Prelude.map T.pack . lines <$> readFile "Day_14/input.txt"

-- Rotate CCW at the start so any load calcs are done left instead of up
part1 :: IO Int
part1 = load . shiftLeft <$> getRocks

part2 :: IO Int
part2 = evalWithCache . simul 0 <$> getRocks
  where
    simul :: Int -> Rocks -> Cached Int
    simul i rs = do
      curr <- getCacheR rs
      case curr of
        Just s  -> getCacheI $ s + (1000000000 - i) `mod` (i - s) 
        Nothing -> putCache rs i >> simul (i+1) (cyc rs)