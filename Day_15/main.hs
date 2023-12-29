
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char
import Control.Monad.Trans.State
import Data.Array

hash :: Text -> Int
hash = T.foldl (\x c -> ((x + ord c) * 17) `mod` 256) 0

bits :: IO [Text]
bits = T.split (','==) . T.pack <$> readFile "Day_15/input.txt"

part1 :: IO Int
part1 = sum . map hash <$> bits

type Box = Map String Int
type Boxes = Array Int Box

emptyBoxes :: Boxes
emptyBoxes = undefined

part2 :: IO Int
part2 = undefined