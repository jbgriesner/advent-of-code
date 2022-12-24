module Day23 (solve_day23) where


import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)
import Utils

import Data.Function (on)
import Data.Ord (comparing)
import Data.List (sortBy, groupBy)
import Data.Set (Set, member, notMember, insert, delete, foldr, fromList, size, findMin, findMax, map)

parseInput :: String -> Set (Int, Int)
parseInput input = fromList $ concat [[(r, c) | (c, t) <- zip [0 .. ] line, t == '#'] 
                                              | (r, line) <- zip [0 .. ] $ lines input]

getNextMove :: Int -> Set (Int, Int) -> (Int, Int) -> [((Int, Int), (Int, Int))]
getNextMove state elves elf@(r, c) | not shouldMove = [               ]
                                   | otherwise      = [(elf, proposed)]
                             where surrounded = any (`member` elves) [(r', c') | r' <- [r - 1 .. r + 1],
                                                                                 c' <- [c - 1 .. c + 1],
                                                                                 (r', c') /= (r, c)]
                                   moveNorth  = all (`notMember` elves) [(r - 1, c'   ) | c' <- [c - 1 .. c + 1]]
                                   moveSouth  = all (`notMember` elves) [(r + 1, c'   ) | c' <- [c - 1 .. c + 1]]
                                   moveWest   = all (`notMember` elves) [(r'   , c - 1) | r' <- [r - 1 .. r + 1]]
                                   moveEast   = all (`notMember` elves) [(r'   , c + 1) | r' <- [r - 1 .. r + 1]]
                                   shouldMove = surrounded && or [moveNorth, moveSouth, moveWest, moveEast]
                                   directions = zip [moveNorth, moveSouth, moveWest, moveEast] [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
                                   directions'= drop state directions ++ take state directions
                                   proposed   = snd . head . filter fst $ directions'

nextState :: (Int, Set (Int, Int)) -> (Int, Set (Int, Int))
nextState (state, elves) = (newState, foldl (\elves' (old, new) -> insert new $ delete old elves') elves newPositions')
    where newPositions   = Data.Set.foldr (\elf -> (getNextMove state elves elf ++)) [] elves
          newPositions'  = concat . filter ((== 1) . length) . groupBy (on (==) snd) . sortBy (comparing snd) $ newPositions
          newState       = (state + 1) `mod` 4

countEmptyTiles :: Set (Int, Int) -> Int
countEmptyTiles elves = width * height - size elves
    where minR = fst . findMin $ elves
          maxR = fst . findMax $ elves
          minC = findMin . Data.Set.map snd $ elves
          maxC = findMax . Data.Set.map snd $ elves
          (width, height) = (maxC - minC + 1, maxR - minR + 1)
 
dayNum :: Int
dayNum = 23

run :: String -> IO ()
run s = do
    let input = parseInput s
    print $ countEmptyTiles . snd . (!! 10) . iterate nextState $ (0, input)
    print $ (+ 1) . length . takeWhile (\(s, i) -> i /= (snd $ nextState (s, i))) . iterate nextState $ (0, input)
    -- runStdoutLoggingT (code 1 s) >>= (\x -> putStrLn $ "     part 1: " <> show x)
    -- runStdoutLoggingT (code 2 s) >>= (\x -> putStrLn $ "     part 2: " <> show x)

solve_day23 :: IO ()
solve_day23 = do
    divide dayNum
    s <- input dayNum
    -- s <- inputest
    run s
