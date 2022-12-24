module Day20 (solve_day20) where

import Data.List
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)
import Utils (input, inputest, Parse (..), Parser (..), charP)
import Utils
import Data.List (maximumBy)
import Data.Ord (comparing)

import Control.Monad
import Control.Monad.State
import Data.Foldable
import Data.Hashable
import Data.HashMap.Strict (HashMap, (!), (!?))
import qualified Data.HashMap.Strict as M
import qualified Data.List            as L
import Data.Maybe
import Data.Traversable
import Text.Printf

import qualified Data.Vector as V


import Data.Bits
import Data.Int
import Data.List (foldl', foldl1')
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import Data.Word
import Data.Char
import GHC.Base (Alternative((<|>)), magicDict)


dayNum :: Int
dayNum = 20

-- |

puzzleInput :: IO (V.Vector (Int,Int))
puzzleInput = (V.indexed . V.fromList).(fmap read).lines <$> readFile "input"

insertToIndex :: Int -> (Int, Int) -> V.Vector (Int, Int) -> V.Vector (Int, Int)
insertToIndex i x xs = before V.++ (V.cons x after)
    where
        (before, after) = V.splitAt i xs 
mix :: Int -> V.Vector (Int, Int) -> V.Vector (Int, Int) -> V.Vector (Int, Int)
mix lp1 pps newList
    | V.null pps = newList
    | otherwise = mix lp1 ps mixedVec 
    where
        Just currentIndex = V.findIndex (\(b,_) -> b==y) newList
        newIndex = mod (x + currentIndex) lp1
        mixedVec = insertToIndex newIndex p (b V.++ (V.tail a))
        (b,a) = V.splitAt currentIndex newList
        p@(y,x) = V.head pps
        ps = V.tail pps
mixNTimes :: Int -> V.Vector (Int, Int) -> V.Vector (Int, Int) -> V.Vector (Int, Int)
mixNTimes 0 _ mixedVec = mixedVec
mixNTimes n originalVec mixedVec = mixNTimes (n-1) originalVec (mix (V.length originalVec -1) originalVec mixedVec)

checkSum :: V.Vector (Int, Int) -> Int
checkSum xs = sum (fmap (snd.(xs V.!)) checkSumIndex)
    where
        Just indexZero = V.findIndex (\(_,a) -> a == 0) xs
        checkSumIndex = fmap (\x -> mod (x+indexZero) (V.length xs)) (V.fromList [1000,2000,3000])

q1 ::  V.Vector (Int, Int) -> Int        
q1 ps = checkSum (mixNTimes 1 ps ps)

-- |


blueprints :: String -> [Blueprint]
blueprints = map bp . lines
    where bp line =
              case getInts line of
                [num, oreBotOre, clayBotOre, obsBotOre, obsBotClay, geodeBotOre, geodeBotObs] ->
                    let costs = map V.fromList [ [0, geodeBotObs, 0, geodeBotOre]
                                               , [0, 0, obsBotClay, obsBotOre]
                                               , [0, 0, 0, clayBotOre]
                                               , [0, 0, 0, oreBotOre] ]
                    in Blueprint num costs $ foldl1' (V.zipWith max) costs
                _ -> error "Malformed input"

sim :: Int32 -> Blueprint -> Int32
sim time (Blueprint _ costs maxCosts) = dfs 0 time (V.fromList [0, 0, 0, 0]) (V.fromList [0, 0, 0, 1]) 0
    where dfs :: Int32 -> Int32 -> Vector Int32 -> Vector Int32 -> Word8 -> Int32
          dfs result t amts bots bans
              | t == 0 = max result $ amts Data.Vector.Unboxed.! 0
              | upperBd <= result = result
              | otherwise = let (res, bns) = foldl' go (result, bans) $ zip [0..] costs
                            in dfs res (t-1) (V.zipWith (+) amts bots) bots bns
              where upperBd = amts Data.Vector.Unboxed.! 0 + t*(bots Data.Vector.Unboxed.! 0) + t*(t+1) `div` 2
                    go (res, bns) (i, cost)
                        | bns .&. shiftL 1 i == 0 &&
                          (i == 0 || bots Data.Vector.Unboxed.! i < maxCosts Data.Vector.Unboxed.! i) &&
                          V.and (V.zipWith (>=) amts cost) =
                              ( dfs res (t-1) (V.zipWith3 (\a b c -> a + b - c) amts bots cost)
                                        (V.zipWith (+) bots $ V.replicate 4 0 V.// [(i, 1)]) 0
                              , bns .|. shiftL 1 i )
                        | otherwise = (res, bns)

part1 :: String -> Int32
part1 = sum . map (\b@(Blueprint n _ _) -> n * sim 24 b) . blueprints

part2 :: String -> Int32
part2 = product . map (sim 32) . take 3 . blueprints

--

code :: MonadLogger m => Int -> String -> m Int32
code k s
    | k == 1 = do
        return $ part1 s
    | otherwise = do
        return $ part2 s

run :: String -> IO ()
run s = do
    runStdoutLoggingT (code 1 s) >>= (\x -> putStrLn $ "     part 1: " <> show x)
    runStdoutLoggingT (code 2 s) >>= (\x -> putStrLn $ "     part 2: " <> show x)

solve_day20 :: IO ()
solve_day20 = do
    divide dayNum
    s <- input dayNum
    -- s <- inputest
    run s
