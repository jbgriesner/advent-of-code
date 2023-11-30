{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# LANGUAGE NamedFieldPuns #-}
module Day15 (solve_day15) where

import Control.Concurrent
import Algorithm.Search (bfsM)
import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
import Data.Char
import Data.List hiding (tails)
import Utils
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)
import Control.Monad
import qualified Data.Text as T
import Prelude hiding (id, Right, Down, Left, Up)
import qualified Data.Set as S
import Control.Monad.State
import Data.Char (isDigit)
import Control.Applicative (Alternative, empty, (<|>))
import Data.Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Map (Map, (!))

dayNum :: Int
dayNum = 15

type Pos = (Int, Int)
newtype LineType = LineType (Pos, Pos) deriving (Show)
newtype Devices = Devices [LineType] deriving (Show)

data Element = Sensor | Beacon | Air | NoBeacon deriving (Enum, Eq)

instance Show Element where
    show Sensor = "S"
    show Beacon = "B"
    show Air = "."
    show NoBeacon = "X"

newtype Grid = G {
    gridMap :: Map Pos (Pos, Element)
} deriving (Show, Eq)

initGrid :: Grid
initGrid = G Map.empty

distance :: Pos -> Pos -> Int
distance (a, b) (c, d) = abs (a - c) + abs (b - d)

printGrid :: Grid -> (Pos, Pos) -> String
printGrid (G m) ((a, b), (c,d)) =
    let
        l = [(x, y) | x <- [a..c], y <- [d..b]]
        l' = groupBy (\(_,a) (_,b) -> a==b) $ sortOn snd l
    in concatMap (showLine m) l'
        where
            showLine :: Map Pos (Pos, Element) -> [Pos] -> String
            showLine m l = foldl f "" l <> "\n"
                        where
                            f :: String -> Pos -> String
                            f s p =
                                case Map.lookup p m of
                                    Nothing -> ""
                                    Just x -> let s' = reverse s
                                                  s'' = show (snd x) <> s'
                                              in reverse s''

getDimensions :: [(Pos, Pos)] -> (Pos, Pos)
getDimensions ds =
    let
        l = concatMap (\(p1, p2) -> p1:[p2]) ds
        minX = getMinX l
        maxX = getMaxX l
        maxY = getMaxY l
        minY = getMinY l
    in
        ((minX, maxY), (maxX, minY))
        where
            getMinX l = minimum $ map fst l
            getMaxX l = maximum $ map fst l
            getMaxY l = maximum $ map snd l
            getMinY l = minimum $ map snd l

instance Parse LineType where
    parser = LineType <$> (((,) <$> sP) <*> bP)
        where
            sP, bP :: Parser (Int, Int)
            sP = (,)
                        <$> (stringP "Sensor at x=" *> (number <|> negativeNumber) <* stringP ", y=")
                        <*> (number <|> negativeNumber)

            bP = (,)
                        <$> (stringP ": closest beacon is at x=" *> (number <|> negativeNumber) <* stringP ", y=")
                        <*> (number <|> negativeNumber)

            number :: Parser Int
            number = read <$> notNull (spanP isDigit)

            negativeNumber :: Parser Int
            negativeNumber = f <$> notNull (stringP "-" *> spanP isDigit)
                where f ds = (-1) * read ds

parseInput :: String -> Devices
parseInput s = Devices $ map Utils.fromString $ lines s

foldSensors :: Grid -> (Pos, Pos) -> Grid
foldSensors (G m) (p1, p2) = G $ Map.insert p1 (p2, Sensor) $ Map.insert p2 (p2, Beacon) m

-- foldBeacons :: Grid -> (Int, Int) -> Grid
-- foldBeacons (G m) p = G $ Map.insert p Beacon m

fillGrid :: Grid -> (Pos, Pos) -> Grid
fillGrid (G grid) ((a,b), (c,d)) =
    let
        allPos = [(x, y) | x <- [a..c], y <- [d..b]]
     in G $ foldl fillGridWith grid allPos
        where
            fillGridWith :: Map Pos (Pos, Element) -> Pos -> Map Pos (Pos, Element)
            fillGridWith m p =
                case Map.lookup p m of
                  Nothing -> Map.insert p (p, Air) m
                  Just _ -> m

findEmptySpots ::
    Grid        -- main grid of sensors and beacons
    -> [Pos]    -- list of positions of sensors
    -> Grid     -- the result grid
findEmptySpots (G g) ps = G $ foldl f g ps

f :: Map Pos (Pos, Element) -> Pos -> Map Pos (Pos, Element)
f m p = case Map.lookup p m of
    Nothing -> m
    Just (p2, _) -> let dist = distance p2 p
                        pss = getClosestPos p dist
                    in foldl gg m pss

gg :: Map Pos (Pos, Element) -> Pos -> Map Pos (Pos, Element)
gg m' p' = case Map.lookup p' m' of
    Nothing -> m'
    Just (_, Air) -> Map.insert p' (p', NoBeacon) m'
    Just (_, _) -> m'

getClosestPos :: Pos -> Int -> [Pos]
getClosestPos (p1, p2) d =
    let l = [(x,y) | x <- [(p1-d)..(p1+d)], y <- [(p2-d)..(p2+d)]]
        in foldl (\ls p -> if distance p (p1, p2) <= d then p:ls else ls) [] l

getRow :: Int -> Grid -> [(Pos, Element)]
getRow k (G m) =
    let filtered = Map.filterWithKey (\(_, y) _ -> y == k) m
    in map (\(x, z) -> (x, snd z)) $ Map.toList filtered

type Interval = (Int, Int)

excludedCoords :: (MonadLogger m) => Int -> (Pos, Pos) -> m (Maybe Interval)
excludedCoords rowNum (sensor@(sx, sy), beacon) = do
  let dist = distance sensor beacon
  let distToRow = abs (sy - rowNum)
  let leftoverDist = dist - distToRow
  if leftoverDist < 0
    then return Nothing
    else return $ Just (sx - leftoverDist, sx + leftoverDist)

mergeIntervals :: (MonadLogger m) => [Interval] -> m [Interval]
mergeIntervals [] = return []
mergeIntervals intervals = do
  let sorted = sort intervals
  mergeTail [] (head sorted) (tail sorted)
  where
    mergeTail :: (MonadLogger m) => [Interval] -> Interval -> [Interval] -> m [Interval]
    mergeTail accum current [] = return $ reverse (current : accum)
    mergeTail accum current@(cStart, cEnd) (first@(fStart, fEnd) : rest) = if fStart > cEnd
      then mergeTail (current : accum) first rest
      else mergeTail accum (cStart, max cEnd fEnd) rest

countIntervalsExcludingBeacons :: (MonadLogger m) => [Interval] -> [Int] -> m Int
countIntervalsExcludingBeacons intervals beaconXs = countTail 0 intervals (sort beaconXs)
  where
    countTail :: (MonadLogger m) => Int -> [Interval] -> [Int] -> m Int
    countTail accum [] _ = return accum
    countTail accum ((next1, next2) : rest) [] = countTail (accum + (next2 - next1 + 1)) rest []
    countTail accum ints@((next1, next2) : restInts) beacons@(nextBeaconX : restBeacons)
      | nextBeaconX < next1 = countTail accum ints restBeacons
      | nextBeaconX > next2 = countTail (accum + (next2 - next1)) restInts restBeacons
      | otherwise = countTail (accum - 1) ints restBeacons

processInputHard :: (MonadLogger m) => [(Pos, Pos)] -> Int -> m (Maybe Pos)
processInputHard inputs maxDimen = evaluateRow 0
  where
    evaluateRow :: (MonadLogger m) => Int -> m (Maybe Pos)
    evaluateRow row = if row > maxDimen then return Nothing
      else do
        resultingIntervals <- mapM (excludedCoords row) inputs
        mergedIntervals <- mergeIntervals (catMaybes resultingIntervals)
        result <- findHole mergedIntervals maxDimen
        case result of
          Nothing -> evaluateRow (row + 1)
          Just col -> return $ Just (col, row)

findHole :: (MonadLogger m) => [Interval] -> Int -> m (Maybe Int)
findHole [] _ = return Nothing
findHole [(start, end)] maxCol
  | start > 0 = return (Just (start - 1))
  | end < maxCol = return (Just (end + 1))
  | otherwise = return Nothing
findHole ((start1, end1) : (start2, end2) : rest) maxCol = if end1 + 1 < start2 && (end1 + 1) >= 0 && (end1 + 1) <= maxCol
  then return (Just (end1 + 1))
  else findHole ((start2, end2) : rest) maxCol

findHardSolution :: (MonadLogger m) => Maybe Pos -> m (Maybe Int)
findHardSolution Nothing = return Nothing
findHardSolution (Just (col, row)) = return $ Just $ fromIntegral col * 4000000 + fromIntegral row

code :: MonadLogger m => Int -> String -> m Int
code k s
    | k == 1 = do
        let (Devices devices) = parseInput s
        let devices' = map (\(LineType x) -> x) devices

        resultingIntervals <- mapM (excludedCoords 2000000) devices'
        mergedIntervals <- mergeIntervals (catMaybes resultingIntervals)
        let beacons = nub $ filter (\c@(_, y) -> y == 2000000) (snd <$> devices')
        countIntervalsExcludingBeacons mergedIntervals (fst <$> beacons)

    | otherwise = do
        let (Devices devices) = parseInput s
        let devices' = map (\(LineType x) -> x) devices
        logDebugN (T.pack $ "\n        devices': " <> show devices')
        rez <- processInputHard devices' 4000000
        logDebugN (T.pack $ "\n        rez: " <> show rez)
        r <- findHardSolution rez
        logDebugN (T.pack $ "\n        r: " <> show r)
        case r of
            Nothing -> return 0
            Just x -> return x

    -- let devices' = map (\(LineType x) -> x) devices
    -- let sensors = map fst devices'
    -- let dims = getDimensions devices'
    -- -- logDebugN (T.pack $ "\n        devices': " <> show devices')
    -- -- logDebugN (T.pack $ "\n        dimensions: " <> show dims)
    -- let grid = foldl foldSensors initGrid devices'
    -- -- logDebugN (T.pack $ "\n        grid': " <> show grid')
    -- let grid' = fillGrid grid dims
    -- -- -- logDebugN (T.pack $ "\n        grid'': " <> show grid'')
    -- -- logDebugN (T.pack $ "\n        printed grid: \n" <> printGrid grid' dims)
    -- -- logDebugN (T.pack $ "\n\n")
    -- let grid'' = findEmptySpots grid' sensors
    -- -- logDebugN (T.pack $ "\n        printed grid: \n" <> printGrid grid'' dims)
    -- -- logDebugN (T.pack $ "\n        sensors: \n" <> show sensors)
    -- -- let row = getRow 10 grid''
    -- let row = getRow 2000000 grid''
    -- -- logDebugN (T.pack $ "\n        row: \n" <> show row)
    -- -- return 1
    -- return $ foldl (\k el -> case snd el of
    --                         NoBeacon -> k+1
    --                         _ -> k) 0 row

run :: String -> IO ()
run s = do
    runStdoutLoggingT (code 1 s) >>= (\x -> putStrLn $ "     part 1: " <> show x)
    runStdoutLoggingT (code 2 s) >>= (\x -> putStrLn $ "     part 2: " <> show x)

solve_day15 :: IO ()
solve_day15 = do
    divide dayNum
    s <- input dayNum
    -- s <- inputest
    run s
