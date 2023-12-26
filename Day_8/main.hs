import qualified Data.Map as M

type Node = (String,String)

parseLine :: [String] -> (String,Node)
parseLine [name,_,_:l,r] = (name,(init l, init r))

move :: Char -> Node -> String
move 'L' = fst
move 'R' = snd

follow :: String -> String -> M.Map String Node -> Int
follow _ "ZZZ" _ = 0
follow (i:is) c m = 1 + follow is (move i $ m M.! c) m

follow2 :: String -> String -> M.Map String Node -> [String]
follow2 _ (_:_:"Z") _ = []
follow2 (i:is) c m = c : follow2 is (move i $ m M.! c) m

part1 :: IO Int
part1 = do
    (instrs:_:rest) <- lines <$> readFile "Day_8/input.txt"
    return $ follow (cycle instrs) "AAA" $ M.fromList . map (parseLine . words) $ rest

part2 :: IO Int
part2 = do
    (instrs:_:rest) <- lines <$> readFile "Day_8/input.txt"
    let m = M.fromList . map (parseLine . words) $ rest
        starters = filter (('A'==) . last) $ M.keys m
    return $ foldr (lcm . length . flip (follow2 (cycle instrs)) m) 1 starters
