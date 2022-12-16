{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Utils where

import qualified Data.List as L
import Control.Applicative (Alternative (empty, many, some, (<|>)), (<**>))
import Data.Char           (isSpace, isDigit)

-- | data input function for each Day module
input :: Int -> IO String
input dayNum = readFile $ "./data/input_day" <> show dayNum
-- input = readFile $ "./data/test"

-- | data input function for each Day module with example data
inputest :: IO String
inputest = readFile $ "./data/test"

-- | get product of 2 max elements of a list
get2Max :: (Num a, Eq a, Ord a) => [a] -> a
get2Max l = let l' = take 2 $ reverse $ L.sort l
            in foldl (\x y -> x*y) 1 l'

-- | output a separator with the result on stdout
divide :: Int -> IO ()
divide s = putStrLn $ "--- result of day " <> show s <> " is:"

-- | simply remove duplicate values from a list
rmdups :: (Ord a) => [a] -> [a]
rmdups = L.map L.head . L.group . L.sort

newtype Parser a = Parser {
  runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> do
    (x, s') <- p s
    Just (f x, s')

instance Applicative Parser where
   pure x = Parser $ \s -> Just (x, s)

   Parser p1 <*> Parser p2 = Parser $ \s -> do
      (f, s') <- p1 s
      (x, s'') <- p2 s'
      Just (f x, s'')

instance Alternative Parser where
   empty = Parser $ \_ -> Nothing

   Parser p1 <|> Parser p2 = Parser $ \s -> p1 s <|> p2 s

-- |check if Parser not null
notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \s -> do
  (xs, s') <- p s
  if null xs
    then Nothing
    else Just (xs, s')

-- | parse a string while true
spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \s -> let (token, rest) = span f s
  in Just (token, rest)

stringP :: String -> Parser String
stringP = traverse charP

-- | Parse a single char
charP :: Char -> Parser Char
charP c = Parser f
  where
    f (y:ys)
      | y == c = Just (c, ys)
      | otherwise = Nothing
    f [] = Nothing

-- | Parser Op combinator
(>==>) :: Parser a -> Parser (a -> a -> a) -> Parser (a -> a)
(Parser p1) >==> (Parser p2) = Parser $ \input ->
     case p1 input of
      Just (x, input') -> case p2 input' of
          Just (f, input'') -> Just (f x, input'')
          Nothing -> Nothing
      Nothing -> Nothing

-- | Apply Parsed function
($$) :: Parser (a -> a) -> Parser a -> Parser a
(Parser p1) $$ (Parser p2) = Parser $ \input ->
  case p1 input of
    Just (f, input') -> case p2 input' of
      Just (x, input'') -> Just (f x, input'')
      Nothing -> Nothing
    Nothing -> Nothing

-- | type class for Parser
class Parse a where
    parser :: Parser a

    fromString :: String -> a
    fromString cs =
        case runParser parser cs of
            Just(s, []) -> s
            Just(s, cs) -> error ("cannot parse input: ’"++cs++"’")
            Nothing -> error "Nothing"

    -- toString :: a -> String
