{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-dodgy-imports #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies, ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
module Day16 (solve_day16) where

import Control.Arrow (second)
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
import Data.Map ((!?), assocs, filter, fromList, fromSet, insertWith, keys, union, withoutKeys, Map, (!))
import Data.Char           (isAlpha)
import qualified Data.Set as Set (empty, foldl', fromList, insert, mapMonotonic, member, notMember)
import qualified Data.Heap as Heap (FstMaxPolicy, insert, singleton, view)
import qualified Data.IntMap as IntMap (assocs, fromListWith, singleton, unionWith)
import Data.Containers.ListUtils (nubOrd)
import Data.Semigroup (Sum(Sum, getSum))
import Debug.Trace (traceShow)


dayNum :: Int
dayNum = 16

-- | Parse Input Tools
newtype Valve = Valve String deriving (Eq)

instance Show Valve where
    show (Valve s) = s

data LineType = L {
    valve :: Valve,
    flow :: Int,
    tunnels :: [Valve]
} deriving (Show)

instance Parse LineType where
    parser = (L <$> valveP) >>> numberP >>> valvesP
        where
            sepBy :: Parser String -> Parser Valve -> Parser [Valve]
            sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

            valvesP :: Parser [Valve]
            valvesP = (stringP "; tunnel " <|> stringP "; tunnels ") *> ((stringP "lead " <|> stringP "leads ") *> ((stringP "to valve "<|> stringP "to valves ") *> valves))
                where
                    valves = sepBy sep vP
                    sep = stringP ", "

            vP :: Parser Valve
            vP = Valve <$> nameP

            valveP :: Parser Valve
            valveP = Valve <$> (stringP "Valve " *> nameP <* stringP " has flow rate=")

            nameP :: Parser String
            nameP = spanP isAlpha

            numberP :: Parser Int
            numberP = read <$> notNull (spanP isDigit)

parseInput :: String -> [LineType]
parseInput s = map Utils.fromString $ lines s

-- | Core Solution
code :: MonadLogger m => Int -> String -> m Int
code k s = do
    let n = if k == 1 then 1 else 2
    let m = if k == 1 then 30 else 26
    let l = map (\(L (Valve v) f t) -> (v, (f, map (\(Valve vv) -> vv) t))) $ parseInput s
    let gr = Map.fromList $ l
    logDebugN (T.pack $ "\n        gr: " <> show gr)
    let distances = fmap getSum . shortestPaths $ Map.fromList [((a, b), Sum 1) | (a, (_, bs)) <- Map.assocs gr, b <- bs]
        next (_, _, _, _, 0) = (Nothing, [])
        next (rooms, valves, flow, total, time)
            | null options = (Just estimate, [(estimate, (rooms, valves, flow, estimate, 0))])
            | otherwise = (Just potential, options)
            where
                estimate = total + flow * time
                potential = estimate + sum
                    [ maximum $ 0 :
                    [ rate * (time - d - 1)
                        | (room, age) <- nubOrd rooms
                        , d <- maybeToList $ subtract age <$> distances Map.!? (room, room')
                        , 0 <= d && d < time
                    ]
                    | (room', rate) <- Map.assocs valves
                    ]
                moves = IntMap.fromListWith (IntMap.unionWith (<>))
                    [ (d, IntMap.singleton i [(room', rate)])
                    | (i, (room, age)) <- zip [0..] rooms
                    , (room', rate) <- Map.assocs valves
                    , d <- maybeToList $ subtract age <$> distances Map.!? (room, room')
                    , 0 <= d && d < time
                    ]
                options =
                    [ ( estimate + rate * (time - d - 1)
                        , ( sort $ (second (d + 1 +) <$> rooms) // zip is ((, 0) <$> rooms')
                        , Map.withoutKeys valves $ Set.fromList rooms'
                        , flow + rate
                        , total + flow * (d + 1)
                        , time - d - 1
                        )
                        )
                    | (d, moves') <- IntMap.assocs moves
                    , (is, moves'') <- fmap unzip . filterM (const [False, True]) $ IntMap.assocs moves'
                    , moves'''@(_:_) <- sequence moves''
                    , let (rooms', sum -> rate) = unzip moves'''
                    , and . zipWith Set.notMember rooms' $ scanl' (flip Set.insert) Set.empty rooms'
                    ]

        max' total (rooms, valves, flows, total', time)
            | total' > total = traceShow (rooms, Map.keys valves, flows, total', time) total'
            | otherwise = total

    pure . foldl' max' 0 $ search next (0, (replicate n ("AA", 0), Map.filter (> 0) $ fst <$> gr, 0, 0, m))

search :: (Ord a, Ord b) => (a -> (Maybe b, [(b, a)])) -> (b, a) -> [a]
search next = search' Set.empty Nothing . Heap.singleton @Heap.FstMaxPolicy where
    search' seen bestEstimate (Heap.view -> Just ((b, a), heap))
      | Set.member a seen = search' seen bestEstimate heap
      | fromMaybe False $ (<) <$> potential <*> bestEstimate = search' seen bestEstimate heap
      | otherwise = a : search' seen' (Just $ maybe b (max b) bestEstimate) heap' where
            (potential, nexts) = next a
            seen' = Set.insert a seen
            heap' = foldl' (flip Heap.insert) heap $ Prelude.filter (flip Set.notMember seen' . snd) nexts
    search' _ _ _ = []

shortestPaths :: (Ord a, Monoid b, Ord b) => Map (a, a) b -> Map (a, a) b
shortestPaths es =
    Set.foldl' (\d a -> Set.foldl' (\d b -> Set.foldl' (flip $ update a b) d vs) d vs) d0 vs
  where
    vs = Set.fromList $ concat [[a, b] | (a, b) <- Map.keys es]
    d0 = Map.union es $ Map.fromSet (const mempty) $ Set.mapMonotonic (join (,)) vs
    update c b a d
      | Just x <- d Map.!? (a, c)
      , Just y <- d Map.!? (c, b)
      = Map.insertWith min (a, b) (x <> y) d
      | otherwise = d

(//) :: [a] -> [(Int, a)] -> [a]
as // ias = update (zip [0..] as) ias where
    update as@((i, a):as') bs@((j, b):bs') = case compare i j of
        LT -> a:update as' bs
        EQ -> b:update as' bs
        GT -> update as bs'
    update as _ = snd <$> as
infixl 9 //

run :: String -> IO ()
run s = do
    runStdoutLoggingT (code 1 s) >>= (\x -> putStrLn $ "     part 1: " <> show x)
    runStdoutLoggingT (code 2 s) >>= (\x -> putStrLn $ "     part 2: " <> show x)

solve_day16 :: IO ()
solve_day16 = do
    divide dayNum
    s <- input dayNum
    -- s <- inputest
    run s
