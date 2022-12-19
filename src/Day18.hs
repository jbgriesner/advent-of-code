module Day18 (solve_day18) where

import Data.List

import Data.Set (Set, member, notMember, delete, fromList, findMin, findMax, union, foldl)
import qualified Data.Set as S (map)
import Utils (input, inputest, divide, Parse (..), Parser (..), notNull, charP, spanP, stringP)
import Control.Monad.Logger (MonadLogger, runStdoutLoggingT, logDebugN)
import Data.Char
import Control.Applicative ((<|>))
import Data.Set (Set, empty, singleton, insert, member, size)
import Data.List (foldl')

dayNum :: Int
dayNum = 18

type Coord = (Int, Int, Int)
type Cube = Coord

surfaceArea :: [Coord] -> Int
surfaceArea points = foldl' (\acc p -> acc + (sides p points)) 0 points
  where
    sides :: Coord -> [Coord] -> Int
    sides (x, y, z) ps =
         (if (x-1, y, z) `elem` ps then 0 else 1) +
         (if (x+1, y, z) `elem` ps then 0 else 1) +
         (if (x, y-1, z) `elem` ps then 0 else 1) +
         (if (x, y+1, z) `elem` ps then 0 else 1) +
         (if (x, y, z-1) `elem` ps then 0 else 1) +
         (if (x, y, z+1) `elem` ps then 0 else 1)

newtype LineType = C Coord deriving (Eq, Show)

instance Parse LineType where
    parser = C <$> ((,,) <$> (numP <* charP ',') <*> (numP <* charP ',') <*> numP)
        where
            numP :: Parser Int
            numP = number <|> negativeNumber

            number, negativeNumber :: Parser Int
            number = f <$> notNull (spanP isDigit)
                where f ds = read ds

            negativeNumber = f <$> notNull (stringP "-" *> spanP isDigit)
                where f ds = (-1) * read ds

parseInput :: String -> [(Int, Int, Int)]
parseInput s = map ((\(C c) -> c) . Utils.fromString) $ lines s

surfaceExterieure1 :: [Cube] -> Cube -> Int
surfaceExterieure1 cubes (x, y, z) =
          let
              allAdjacents = [(x+1, y, z),(x-1, y, z),(x, y+1, z),(x, y-1, z),(x, y, z+1),(x, y, z-1)]
          in sum [if elem aa cubes then 0 else 1 | aa <- allAdjacents]

surfaceExterieureTotale :: [Cube] -> Int
surfaceExterieureTotale cubes = sum (map (surfaceExterieure1 cubes) cubes)

getNeighbours :: Set (Int, Int, Int) -> (Int, Int, Int) -> [(Int, Int, Int)]
getNeighbours world (x, y, z) = filter (`member` world) [(x - 1, y, z), (x + 1, y, z), (x, y - 1, z), (x, y + 1, z), (x, y, z - 1), (x, y, z + 1)]

getSurface :: Set (Int, Int, Int) -> Int
getSurface world = Data.Set.foldl (flip $ (+) .  (6 -) .  length . getNeighbours world) 0 world

getNegativeSpace :: Set (Int, Int, Int) -> Set (Int, Int, Int)
getNegativeSpace world = fromList [(x, y, z) | x <- [minX .. maxX], y <- [minY .. maxY], z <- [minZ .. maxZ], (x, y, z) `notMember` world]
    where xs = S.map (\(x, _, _) -> x) world
          ys = S.map (\(_, y, _) -> y) world
          zs = S.map (\(_, _, z) -> z) world
          (minX, minY, minZ) = (findMin xs - 1, findMin ys - 1, findMin zs - 1)
          (maxX, maxY, maxZ) = (findMax xs + 1, findMax ys + 1, findMax zs + 1)

getInside :: Set (Int, Int, Int) -> [(Int, Int, Int)] -> Set (Int, Int, Int)
getInside negative [] = negative
getInside negative (el:queue) = getInside negative' queue'
    where neighbours = getNeighbours negative el
          negative'  = Data.List.foldl (flip Data.Set.delete) negative neighbours
          queue'     = queue ++ neighbours

code :: MonadLogger m => Int -> String -> m (Int, Int)
code k s
    | k == 1 = do
        let cubes = parseInput s
        return $ (surfaceExterieureTotale cubes, surfaceArea cubes)
    | otherwise = do
        let cubes' = Data.Set.fromList $ parseInput s
        let negative = getNegativeSpace cubes'
        let start = findMin negative
        let inside = getInside (Data.Set.delete start negative) [start]
        let lavaDrop = cubes' `Data.Set.union` inside
        return $ (getSurface cubes', getSurface lavaDrop)

run :: String -> IO ()
run s = do
    runStdoutLoggingT (code 1 s) >>= (\x -> putStrLn $ "     part 1: " <> show x)
    runStdoutLoggingT (code 2 s) >>= (\x -> putStrLn $ "     part 2: " <> show x)

solve_day18 :: IO ()
solve_day18 = do
    divide dayNum
    s <- input dayNum
    -- s <- inputest
    run s

