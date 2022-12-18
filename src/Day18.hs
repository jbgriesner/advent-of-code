module Day18 (solve_day18) where

import Data.List

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
import Data.Set (Set, member, notMember, delete, fromList, findMin, findMax, union, foldl)
import qualified Data.Set as S (map)
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
-- A 3D point
type Point = Coord 

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

-- Calculate the surface area of the droplet.
surfaceArea2 :: [Point] -> Int
surfaceArea2 points = foldl' (\acc p -> acc + sides p points) 0 points
  where
    minX = minimum $ map (\(a, _, _) -> a) points
    minY = minimum $ map (\(_, a, _) -> a) points
    minZ = minimum $ map (\(_, _, a) -> a) points

    maxX = maximum $ map (\(a, _, _) -> a) points
    maxY = maximum $ map (\(_, a, _) -> a) points
    maxZ = maximum $ map (\(_, _, a) -> a) points

    sides :: Point -> [Point] -> Int
    sides (x, y, z) ps =
         (if (x-1, y, z) `elem` ps then 0 else if null $ intersec [(a, y, z) | a <- [minX..(x-2)]] ps then 1 else 0) +
         (if (x+1, y, z) `elem` ps then 0 else if null $ intersec [(a, y, z) | a <- [(x+2)..maxX]] ps then 1 else 0) +
         (if (x, y-1, z) `elem` ps then 0 else if null $ intersec [(x, a, z) | a <- [minY..(y-2)]] ps then 1 else 0) +
         (if (x, y+1, z) `elem` ps then 0 else if null $ intersec [(x, a, z) | a <- [(y+2)..maxY]] ps then 1 else 0) +
         (if (x, y, z-1) `elem` ps then 0 else if null $ intersec [(x, y, a) | a <- [minZ..(z-2)]] ps then 1 else 0) +
         (if (x, y, z+1) `elem` ps then 0 else if null $ intersec [(x, y, a) | a <- [(z+2)..maxZ]] ps then 1 else 0)

intersec :: Eq a => [a] -> [a] -> [a]
intersec xs ys = xs \\ (xs \\ ys)

-- Calculate the exterior surface area of the droplet.
exteriorSurfaceArea :: [Point] -> Int
exteriorSurfaceArea points =
  let
    -- Create a set of all points
    allPoints = foldl' (\acc p -> Data.Set.insert p acc) empty points

    -- Create a set of points on the exterior of the droplet
    exteriorPoints = foldl' (\acc p -> Data.Set.insert p acc) empty (exteriorNeighbors points)

    -- Remove points on the exterior of the droplet that are also within the droplet
    exteriorPoints' = foldl' (\acc p -> if p `member` allPoints then acc else Data.Set.insert p acc) empty exteriorPoints
  in size exteriorPoints'

-- Find all points on the exterior of the droplet.
exteriorNeighbors :: [Point] -> [Point]
exteriorNeighbors points = concatMap exteriorNeighbors' points
  where
    exteriorNeighbors' :: Point -> [Point]
    exteriorNeighbors' (x, y, z) =
      filter (\p -> p `notElem` points) [(x-1, y, z), (x+1, y, z), (x, y-1, z), (x, y+1, z), (x, y, z-1), (x, y, z+1)]

-- data Move = Left | Right deriving (Eq)

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


-- Détermine si deux coordonnées sont adjacentes
adjacent :: Coord -> Coord -> Bool
adjacent (x1, y1, z1) (x2, y2, z2) =  
     (abs (x1 - x2) <= 1 && y1 == y2 && z1 == z2) 
  || (abs (y1 - y2) <= 1 && x1 == x2 && z1 == z2) 
  || (abs (z1 - z2) <= 1 && y1 == y2 && x1 == x2)

-- Calcule la surface extérieure d'un cube donné
surfaceExterieure0 :: [Cube] -> Cube -> [Int]
surfaceExterieure0 cubes (x, y, z) = [if any (adjacent (x', y', z')) cubes then 0 else 1 | x' <- [x-1..x+1], y' <- [y-1..y+1], z' <- [z-1..z+1], x' /= x || y' /= y || z' /= z]

surfaceExterieure1 :: [Cube] -> Cube -> Int
surfaceExterieure1 cubes (x, y, z) = 
          let
              allAdjacents = [(x+1, y, z),(x-1, y, z),(x, y+1, z),(x, y-1, z),(x, y, z+1),(x, y, z-1)]
          in sum [if elem aa cubes then 0 else 1 | aa <- allAdjacents] 


surfaceExterieure :: [Cube] -> Cube -> Int
surfaceExterieure cubes (x, y, z) = sum [if any (adjacent (x', y', z')) cubes then 0 else 1 | x' <- [x-1..x+1], y' <- [y-1..y+1], z' <- [z-1..z+1], (x' /= x || y' /= y || z' /= z) && (x' >= 0 && y' >= 0 && z' >= 0) && (x' <= 20 && y' <= 20 && z' <= 20)]


-- Calcule la surface extérieure de tous les cubes de la grille
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
    -- runStdoutLoggingT (code 1 s) >>= (\x -> putStrLn $ "     part 1: " <> show x)
    runStdoutLoggingT (code 2 s) >>= (\x -> putStrLn $ "     part 2: " <> show x)

solve_day18 :: IO ()
solve_day18 = do
    divide dayNum
    s <- input dayNum
    -- s <- inputest 
    run s

