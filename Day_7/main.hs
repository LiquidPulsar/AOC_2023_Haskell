import Data.List
import Control.Monad
import Control.Arrow
import Control.Applicative
import Data.Ord
import Data.Maybe

data Card = ONE | TWO | THREE | FOUR | FIVE | SIX | SEVEN | EIGHT | NINE | TEN | JACK | QUEEN | KING | ACE deriving (Show, Eq, Ord, Enum)
data Type = HIGH_CARD | PAIR | TWO_PAIR | THREE_OF_A_KIND | FULL_HOUSE | FOUR_OF_A_KIND | FIVE_OF_A_KIND deriving (Show, Eq, Ord, Enum)

type Cards = [Card]
type Hand = (Type,Cards)
type Bet = Int

counts :: Cards -> [(Card,Int)]
counts = sortOn (Down . snd) . map (liftA2 (,) head length) . group . sort

countToType :: [(Card,Int)] -> Type
countToType ((_,5):_) = FIVE_OF_A_KIND
countToType ((_,4):_) = FOUR_OF_A_KIND
countToType ((_,3):(_,2):_) = FULL_HOUSE
countToType ((_,3):_) = THREE_OF_A_KIND
countToType ((_,2):(_,2):_) = TWO_PAIR
countToType ((_,2):_) = PAIR
countToType ((_,1):_) = HIGH_CARD
countToType _ = error "Invalid hand"

getRating1 :: Cards -> Hand
getRating1 = ap (flip (,)) (countToType . counts)

getRating2 :: Cards -> Hand
getRating2 [JACK,JACK,JACK,JACK,JACK] = (FIVE_OF_A_KIND,[JACK])
getRating2 cs = (t,cs)
  where
    cnts = counts cs
    ((a,b):rest) = filter ((/= JACK) . fst) cnts
    jcnt = fromMaybe 0 $ lookup JACK cnts
    t = countToType $ (a,jcnt+b):rest

p2CardScore :: Card -> Int
p2CardScore JACK = -1
p2CardScore c = fromEnum c

parseCard :: Char -> Card
parseCard '2' = TWO
parseCard '3' = THREE
parseCard '4' = FOUR
parseCard '5' = FIVE
parseCard '6' = SIX
parseCard '7' = SEVEN
parseCard '8' = EIGHT
parseCard '9' = NINE
parseCard 'T' = TEN
parseCard 'J' = JACK
parseCard 'Q' = QUEEN
parseCard 'K' = KING
parseCard 'A' = ACE
parseCard c = error $ "Couldn't parse " ++ c : " as a card" 

parseCards :: String -> (Cards,Bet)
parseCards = (map parseCard *** read) . break (== ' ')

part1 :: IO Int
part1 = sum . zipWith (\a b -> a * snd b) [1..] . sort . map (first getRating1 . parseCards) . lines <$> readFile "Day_7/input.txt"

part2 :: IO Int
part2 = sum . zipWith (\a b -> a * snd b) [1..] . sortOn (first $ second (map p2CardScore)) . map (first getRating2 . parseCards) . lines <$> readFile "Day_7/input.txt"