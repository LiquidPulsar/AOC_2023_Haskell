import Data.Char

data Col = Col {r::Int, g::Int, b::Int} deriving Show

-- (||) :: Col -> Col -> Col
-- (Col r1 g1 b1) || (Col r2 g2 b2) = Col (max r1 r2) (max g1 g2) (max b1 b2)

parse :: String -> Col
parse = parse' . dropWhile (/=':')
  where
    parse' :: String -> Col
    parse' [] = Col 0 0 0
    parse' l = case rest of
        ('r':'e':'d':rest')         -> col{r=max d (r col)} where col = parse' rest'
        ('g':'r':'e':'e':'n':rest') -> col{g=max d (g col)} where col = parse' rest'
        ('b':'l':'u':'e':rest')     -> col{b=max d (b col)} where col = parse' rest'
        _ -> error $ ">"++rest++"<"
      where -- skip comma, semicolon, space
        (d',_:rest) = span isDigit $ dropWhile (not . isDigit) l
        d = read d'

good :: String -> Bool
good s = r<=12 && g<=13 && b<=14
  where Col r g b = parse s

pow :: Col -> Int
pow (Col r g b) = r * g * b

part1 :: IO Int
part1 = sum . zipWith (\i j -> if good j then i else 0) [1..] . lines <$> readFile "Day_2/input.txt"

part2 :: IO Int
part2 = sum . map (pow . parse) . lines <$> readFile "Day_2/input.txt"