{-# LANGUAGE BinaryLiterals #-}
module Day17 (solve_day17) where

-- import Control.Arrow (second)
-- import Control.Concurrent
-- import Algorithm.Search (bfsM)
-- import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
-- -- import Data.Char
-- -- import Data.List hiding (tails)
-- import Control.Arrow (second)
-- import Control.Concurrent
-- import Algorithm.Search (bfsM)
-- import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
-- -- import Data.Char
-- -- import Data.List hiding (tails)
-- import Control.Arrow (second)
-- import Control.Concurrent
-- import Algorithm.Search (bfsM)
-- import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
-- -- import Data.Char
-- -- import Data.List hiding (tails)
-- import Control.Arrow (second)
-- import Control.Concurrent
-- import Algorithm.Search (bfsM)
-- import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
-- -- import Data.Char
-- -- import Data.List hiding (tails)
-- import Control.Arrow (second)
-- import Control.Concurrent
-- import Algorithm.Search (bfsM)
-- import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
-- -- import Data.Char
-- -- import Data.List hiding (tails)
-- import Control.Arrow (second)
-- import Control.Concurrent
-- import Algorithm.Search (bfsM)
-- import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
-- -- import Data.Char
-- -- import Data.List hiding (tails)
-- import Control.Arrow (second)
-- import Control.Concurrent
-- import Algorithm.Search (bfsM)
-- import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
-- -- import Data.Char
-- -- import Data.List hiding (tails)
-- import Control.Arrow (second)
-- import Control.Concurrent
-- import Algorithm.Search (bfsM)
-- import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
-- -- import Data.Char
-- -- import Data.List hiding (tails)
-- import Control.Arrow (second)
-- import Control.Concurrent
-- import Algorithm.Search (bfsM)
-- import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
-- -- import Data.Char
-- -- import Data.List hiding (tails)
-- import Control.Arrow (second)
-- import Control.Concurrent
-- import Algorithm.Search (bfsM)
-- import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
-- -- import Data.Char
-- -- import Data.List hiding (tails)
-- import Control.Arrow (second)
-- import Control.Concurrent
-- import Algorithm.Search (bfsM)
-- import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
-- -- import Data.Char
-- -- import Data.List hiding (tails)
-- import Control.Arrow (second)
-- import Control.Concurrent
-- import Algorithm.Search (bfsM)
-- import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
-- -- import Data.Char
-- -- import Data.List hiding (tails)
-- import Control.Arrow (second)
-- import Control.Concurrent
-- import Algorithm.Search (bfsM)
-- import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
-- -- import Data.Char
-- -- import Data.List hiding (tails)
-- import Control.Arrow (second)
-- import Control.Concurrent
-- import Algorithm.Search (bfsM)
-- import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
-- -- import Data.Char
-- -- import Data.List hiding (tails)
-- import Control.Arrow (second)
-- import Control.Concurrent
-- import Algorithm.Search (bfsM)
-- import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
-- -- import Data.Char
-- -- import Data.List hiding (tails)
-- import Control.Arrow (second)
-- import Control.Concurrent
-- import Algorithm.Search (bfsM)
-- import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
-- -- import Data.Char
-- -- import Data.List hiding (tails)
import Utils (input, inputest, Parse (..), Parser (..), charP)
-- import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)
-- -- import Control.Monad
-- -- import qualified Data.Text as T
-- import Prelude hiding (id, Right, Down, Left, Up)
-- import qualified Data.Set as S
-- import Control.Monad.State hiding (state)
-- import Data.Char (isDigit)
-- import Control.Applicative (Alternative, empty, (<|>))
-- import Data.Maybe
-- import Data.HashMap.Strict (HashMap)
-- import qualified Data.HashMap.Strict as HM
-- import Data.List.Split
-- import qualified Data.Map.Strict as Map
-- import Data.Map ((!?), assocs, filter, fromList, fromSet, insertWith, keys, union, withoutKeys, Map, (!))
-- import Data.Char           (isAlpha)
-- import qualified Data.Set as Set (empty, foldl', fromList, insert, mapMonotonic, member, notMember)
-- import qualified Data.Heap as Heap (FstMaxPolicy, insert, singleton, view)
-- import qualified Data.IntMap as IntMap (assocs, fromListWith, singleton, unionWith)
-- import Data.Containers.ListUtils (nubOrd)
-- import Data.Semigroup (Sum(Sum, getSum))
-- import Debug.Trace (traceShow)
-- import qualified Data.IntSet as IntSet (findMax, fromDistinctAscList, fromList, intersection, insert, mapMonotonic, member, notMember, singleton, union)
-- import Data.IntSet (IntSet)
-- import Control.Monad.State
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Bits
import qualified Data.HashMap.Strict as M
import qualified Data.Vector.Unboxed as V
import Data.Word
import Prelude hiding (Right, Left)
import Control.Applicative (many)
import GHC.Base (Alternative((<|>)))
import Control.Monad.Logger

dayNum :: Int
dayNum = 17

-- -- | Parse Input Tools
data Move = Left | Right deriving (Eq)

newtype LineType = Moves [Move]

instance Show Move where
    show Left = "<"
    show Right = ">"

instance Show LineType where
    show (Moves []) = ""
    show (Moves (x:xs)) = show x <> show (Moves xs)

instance Parse LineType where
    parser = Moves <$> repeatP (leftP <|> rightP)
        where
            repeatP :: Parser Move -> Parser [Move]
            repeatP moveP = (:) <$> moveP <*> many moveP <|> pure []

            leftP :: Parser Move
            leftP = charP '<' *> pure Left

            rightP :: Parser Move
            rightP = charP '>' *> pure Right

parseInput :: String -> LineType
parseInput = Utils.fromString

-- | Core Solution
decToBin :: Int -> [Bool]
decToBin n = h $ map g (f n)
    where
        f x = showIntAtBase 2 intToDigit x ""

        g '0' = False
        g '1' = True
        g _ = False

        h l = if length l < 7
            then h (False : l)
            else l

rockToDec :: Rock -> [Int]
rockToDec [] = []
rockToDec rs = map binToDec rs

binToDec :: [Bool] -> Int
binToDec = foldl (\y x -> fromEnum x + 2*y) 0

shiftR :: [Bool] -> [Bool]
shiftR bs = if last bs
    then bs
    else decToBin $ shift (binToDec bs) (-1)

shiftL :: [Bool] -> [Bool]
shiftL bs = if head bs
    then bs
    else decToBin $ shift (binToDec bs) 1

-- moveRock :: Move -> Rock -> Rock
-- moveRock m r = if possibleForAll
--     then map f r
--     else r
--         where
--             possibleForAll = all g r

--             g x  
--                 | x == [False, False, False, False, False, False, False] = True
--                 | otherwise = if m == Right 
--                     then Day17.shiftR x /= x 
--                     else Day17.shiftL x /= x
--             f x
--                 | x == [False, False, False, False, False, False, False] = x
--                 | otherwise = if m == Right then Day17.shiftR x else Day17.shiftL x

type Rock = [[Bool]]

printRock :: Rock -> IO ()
printRock r = print $ Tetris r

-- (..|..) :: [Bool] -> [Bool] -> [Bool]
-- _ ..|.. [] = []
-- [] ..|.. _ = []
-- (x:xs) ..|.. (y:ys) = (x .|. y) : (xs ..|.. ys)

-- (..&..) :: [Bool] -> [Bool] -> [Bool]
-- _ ..&.. [] = []
-- [] ..&.. _ = []
-- (x:xs) ..&.. (y:ys) = (x .&. y) : (xs ..&.. ys)

-- (.>.) :: Rock -> Rock -> Rock
-- r1s .>. r2s = foldr f r2s (reverse r1s)
--     where
--         f :: [Bool] -> Rock -> Rock
--         f bs r = bs >>>> r

-- (>>>>) :: [Bool] -> Rock -> Rock
-- bs >>>> [] = []
-- bs >>>> rr@(r:rs)
--   | r == [False, False, False, False, False, False, False] = [False, False, False, False, False, False, False] : (bs >>>> rs)
--   | or $ r ..&.. bs = bs : rr
--   | otherwise = r : (bs >>>> rs)

-- addRock :: Rock -> State TetriState TetriState
-- addRock r = do
--     (Tetris currentTetris) <- get
--     return $ Tetris $ r .>. currentTetris

horizontalLine :: Rock
horizontalLine = [[False, False, False, False, False, False, False],
                  [False, False, False, False, False, False, False],
                  [False, False, False, False, False, False, False],
                  [False, False, True, True, True, True, False]]

verticalLine :: Rock
verticalLine = [[False, False, True, False, False, False, False],
                [False, False, True, False, False, False, False],
                [False, False, True, False, False, False, False],
                [False, False, True, False, False, False, False]]

crox :: Rock
crox = [[False, False, False, False, False, False, False],
        [False, False, False, True, False, False, False],
        [False, False, True, True, True, False, False],
        [False, False, False, True, False, False, False]]

bigL :: Rock
bigL = [[False, False, False, False, False, False, False],
        [False, False, False, False, True, False, False],
        [False, False, False, False, True, False, False],
        [False, False,  True,  True, True, False, False]]

cube :: Rock
cube = [[False, False, False, False, False, False, False],
        [False, False, False, False, False, False, False],
        [False, False, True, True, False, False, False],
        [False, False, True, True, False, False, False]]

cycleRocks :: [Rock]
cycleRocks = cycle [horizontalLine, crox, bigL, verticalLine, cube]

data TetriState = Tetris {
    grid :: Rock
}

instance Show TetriState where
    show (Tetris []) = ""
    show (Tetris (x:xs)) = "|" <> concatMap (\y -> if y then "#" else ".") x <> "|\n" <> show (Tetris xs)

initRock :: Rock
initRock = [[False, False, False, False, False, False, False],
            [False, False, False, False, False, False, False],
            [False, False, False, False, False, False, False],
            [True, True, True, True, True, True, True]]



code :: MonadLogger m => Int -> String -> m Int
code k s = return 0

run :: String -> IO ()
run s = do
    runStdoutLoggingT (code 1 s) >>= (\x -> putStrLn $ "     part 1: " <> show x)
    -- runStdoutLoggingT (code 2 s) >>= (\x -> putStrLn $ "     part 2: " <> show x)

-- |

rocks :: [[Word8]]
rocks = [ [0b0011110]
        , [0b0001000, 0b0011100, 0b0001000]
        , [0b0000100, 0b0000100, 0b0011100]
        , [0b0010000, 0b0010000, 0b0010000, 0b0010000]
        , [0b0011000, 0b0011000]]

insertRock :: [Word8] -> [Word8] -> [Word8]
insertRock rock grid = dropWhile (==0) $ go rock grid
    where go [] gs = gs
          go (r:rs) (g:gs) = (r .|. g) : go rs gs
          go _ _ = error "Malformed rock or grid"
                                           
solve :: Int -> String -> Int
solve lim input = go M.empty 0 0 0 [0b1111111]
    where dirs = V.fromList input
          go seen addtnlHgt i k befGrid
              | i >= lim = addtnlHgt + length befGrid - 1
              | otherwise = let (k', newGrid) = placeRock k $ rocks !! (i `mod` 5)
                                state = (k' `mod` V.length dirs, i `mod` 5, take 50 grid)
                                seen' = M.insert state (i, length grid - 1) seen
                            in case M.lookup state seen of
                                 Nothing -> go seen' addtnlHgt (i+1) k' newGrid
                                 Just (rockN, height) ->
                                     let hgtDiff = length grid - 1 - height
                                         iDiff = i - rockN
                                         cycles = (lim - i) `div` iDiff
                                     in go seen' (addtnlHgt + cycles*hgtDiff) (i + 1 + cycles*iDiff) k' newGrid
              where grid = replicate (3 + length (rocks !! (i `mod` 5))) 0 ++ befGrid
                    moveLeft rock
                        | any (`testBit` 6) rock = rock
                        | any (/= 0) $ zipWith (.&.) grid rock' = rock
                        | otherwise = rock'
                        where rock' = map (`Data.Bits.shiftL` 1) rock
                    moveRight rock
                        | any (`testBit` 0) rock = rock
                        | any (/= 0) $ zipWith (.&.) grid rock' = rock
                        | otherwise = rock'
                        where rock' = map (`Data.Bits.shiftR` 1) rock
                    placeRock inpIdx rock
                        | all (== 0) $ zipWith (.&.) rock grid = placeRock (inpIdx+1) $ 0 : rock'
                        | otherwise = (inpIdx, insertRock (tail rock) grid)
                        where c = dirs V.! (inpIdx `mod` V.length dirs)
                              rock' = case c of
                                        '<' -> moveLeft rock
                                        '>' -> moveRight rock
                                        _ -> error "Invalid direction"

-- part1 :: String -> Int
-- part1 = solve 2022


solve_day17 :: IO ()
solve_day17 = do
    print "4"
--     divide dayNum
    -- s <- input dayNum
    s <- inputest
    print s
    -- run s
