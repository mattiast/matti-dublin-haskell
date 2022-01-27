module Ch04 where

import Data.Char (digitToInt, isDigit)
import Data.List (foldl')

-- Exercise 1

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_ : xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_ : xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit [x] = Just []
safeInit (x : xs) = (x :) <$> safeInit xs

-- Exercise 2

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p [] = []
splitWith p (x : xs) | not (p x) = splitWith p xs
splitWith p (x : xs) = (x : word) : splitWith p rest
  where
    (word, rest) = go xs
    go [] = ([], [])
    go (y : ys) | p y = (y : r, s) where (r, s) = go ys
    go ys = ([], ys)

test1 = splitWith (\x -> x `mod` 5 /= 0) [1 .. 17] == [[1 .. 4], [6 .. 9], [11 .. 14], [16 .. 17]]

-- ============================

-- Exercise 1

asInt :: String -> Int
asInt ('-' : s) = - asInt s
asInt s = foldl' f 0 s
  where
    f :: Int -> Char -> Int
    f x c | isDigit c = 10*x + digitToInt c
    f _ _ = error "bad character"

test2 = asInt "123" == 123
test3 = asInt "-123" == -123

-- Exercise 3

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

-- Exercise 4

myTakeWhile :: (a -> Bool) -> [a] -> [a]
-- myTakeWhile p l = case l of
--     [] -> []
--     (x:xs) -> if p x then x:myTakeWhile p l else []
myTakeWhile p = foldr f [] where
    f x rest = if p x then x:rest else []

test4 = myTakeWhile (\x -> x `mod` 5 /= 0) [1..10] == [1..4]

-- Exercise 5

-- this is just for (==) for simplicity
myGroupby :: Eq a => [a] -> [[a]]
myGroupby = foldr f [] where
    f x (h@(y:_) : t) = if x == y then (x:h):t else [x]:h:t
    f x t = [x]:t
test5 = myGroupby [1,1,1,2,2,3,2,1,4,5,5] == [[1,1,1],[2,2],[3],[2],[1],[4],[5,5]]
