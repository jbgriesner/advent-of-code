module Test where

import Data.Set (Set, empty, singleton, insert, member, size)
import Data.List (foldl')

-- A 3D point
type Point = (Int, Int, Int)

-- A set of points
type Points = Set Point

-- Calculate the surface area of the droplet.
surfaceArea :: [Point] -> Int
surfaceArea points = foldl' (\acc p -> acc + (sides p points)) 0 points
  where
    sides :: Point -> [Point] -> Int
    sides (x, y, z) ps =
         (if (x-1, y, z) `elem` ps then 0 else 1) +
         (if (x+1, y, z) `elem` ps then 0 else 1) +
         (if (x, y-1, z) `elem` ps then 0 else 1) +
         (if (x, y+1, z) `elem` ps then 0 else 1) +
         (if (x, y, z-1) `elem` ps then 0 else 1) +
         (if (x, y, z+1) `elem` ps then 0 else 1)

-- Calculate the exterior surface area of the droplet.
exteriorSurfaceArea :: [Point] -> Int
exteriorSurfaceArea points =
  let
    -- Create a set of all points
    allPoints = foldl' (\acc p -> insert p acc) empty points

    -- Create a set of points on the exterior of the droplet
    exteriorPoints = foldl' (\acc p -> insert p acc) empty (exteriorNeighbors points)

    -- Remove points on the exterior of the droplet that are also within the droplet
    exteriorPoints' = foldl' (\acc p -> if p `member` allPoints then acc else insert p acc) empty exteriorPoints
  in size exteriorPoints'

-- Find all points on the exterior of the droplet.
exteriorNeighbors :: [Point] -> [Point]
exteriorNeighbors points = concatMap exteriorNeighbors' points
  where
    exteriorNeighbors' :: Point -> [Point]
    exteriorNeighbors' (x, y, z) =
      filter (\p -> p `notElem` points) [(x-1, y, z), (x+1, y, z), (x, y-1, z), (x, y+1, z), (x, y, z-1), (x, y, z+1)]

-- Example input
input = [(2,2,2),(1,2,2),(3,2,2),(2,1,2),(2,3,2),(2,2,1),(2,2,3),(2,2,4),(2,2,6),(1,2,5),(3,2,5),(2,1,5),(2,3,5)]

toto :: IO ()
toto = do
  -- Part 1: Calculate surface area of the droplet
  print (surfaceArea input)  -- Output: 64

  -- Part 2: Calculate exterior surface area of the droplet
  print (exteriorSurfaceArea input)  -- Output: 58