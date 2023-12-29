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

shiftRight :: Rocks -> Rocks
shiftRight = map shiftLine
  where
    shiftLine :: Text -> Text
    shiftLine = T.intercalate "#" . map f . T.split ('#'==)
      where
        f :: Text -> Text
        f = uncurry (flip (<>)) . T.partition ('O'==)

loadLine :: Int -> Text -> Int
loadLine i = (i*) . T.count "O"

load :: Rocks -> Int
load = snd . foldr (\x (i,tot) -> (i+1, tot + loadLine i x)) (1,0)

cyc :: Rocks -> Rocks
cyc = (!!4) . iterate (shiftRight . rotCW)

printB :: Rocks -> IO ()
printB = mapM_ (putStrLn . T.unpack)

getRocks :: IO Rocks
getRocks = map T.pack . lines <$> readFile "Day_14/input.txt"

-- Rotate CCW at the start so any load calcs are done left instead of up
part1 :: IO Int
part1 = load . rotCCW . shiftRight . rotCW <$> getRocks

simul :: Int -> Rocks -> Cached Int
simul i rs = getCacheR rs >>= act
  where
    act (Just s) = getCacheI $ s + (1000000000 - i) `mod` (i - s) 
    act Nothing  = putCache rs i >> simul (i+1) (cyc rs)

part2 :: IO Int
part2 = evalWithCache . simul 0 <$> getRocks