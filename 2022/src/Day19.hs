module Day19 (solve_day19) where

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


import Data.Bits
import Data.Int
import Data.List (foldl', foldl1')
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import Data.Word
import Data.Char
import GHC.Base (Alternative((<|>)), magicDict)


dayNum :: Int
dayNum = 19

data Blueprint = Blueprint Int32 [Vector Int32] (Vector Int32)

data LineType = L [Int32]

instance Parse LineType where
    parser = L <$> (f <$> ((,,,,,,) <$> p1 <*> p2 <*> p3 <*> p4 <*> p5 <*> p6 <*> p7 <* stringP " obsidian."))
        where
            f :: (Int32, Int32, Int32, Int32, Int32, Int32, Int32) -> [Int32]
            f (x1, x2, x3, x4, x5, x6, x7) = x1 : x2 : x3 : x4 : x5 : x6 : x7 : []

            p1 :: Parser Int32
            p1 = stringP "Blueprint " *> (numberP <|> negativeNumber)
            
            p2 :: Parser Int32
            p2 = stringP ": Each ore robot costs " *> (numberP <|> negativeNumber)

            p3 :: Parser Int32
            p3 = stringP " ore. Each clay robot costs " *> (numberP <|> negativeNumber)

            p4 :: Parser Int32
            p4 = stringP " ore. Each obsidian robot costs " *> (numberP <|> negativeNumber)

            p5 :: Parser Int32
            p5 = stringP " ore and " *> (numberP <|> negativeNumber)

            p6 :: Parser Int32
            p6 = stringP " clay. Each geode robot costs " *> (numberP <|> negativeNumber)

            p7 :: Parser Int32
            p7 = stringP " ore and " *> (numberP <|> negativeNumber)

            numberP :: Parser Int32
            numberP = read <$> notNull (spanP isDigit)

            negativeNumber :: Parser Int32
            negativeNumber = f <$> notNull (stringP "-" *> spanP isDigit)
              where f ds = (-1) * read ds

getInts :: String -> [Int32]
getInts s = let (L m) = Utils.fromString s in m

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

solve_day19 :: IO ()
solve_day19 = do
    divide dayNum
    s <- input dayNum
    -- s <- inputest
    run s
