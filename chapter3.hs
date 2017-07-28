module Chapter3 where

-- 1. 
-- :type ['a','b','c']
-- ['a','b','c'] :: [Char]
-- 
-- :type ('a','b','c')
-- ('a','b','c') :: (Char, Char, Char)
--
-- :type [(False, '0'), (True, '1')]
-- [(False, '0'), (True, '1')] :: [(Bool, Char)]
--
-- :type ([False, True], ['0', '1'])
-- ([False, True], ['0', '1']) :: ([Bool], [Char])
--
-- :type (tail, init, reverse)
-- (tail, init, reverse) :: ([a2] -> [a2], [a1] -> [a1], [a] -> [a])
--
-- 2.
bools = [True]
-- :type bools
-- bools :: [Bool]

nums = [[1],[2]]
-- :type nums
-- nums :: [[Integer]]

add x y z = x + y + z 
-- :type add
-- add :: Num a => a -> a -> a -> a

copy x = (x,x)
-- :type copy
-- copy :: t -> (t, t)

apply f x = f x
-- :type apply
-- apply :: (t1 -> t) -> t1 -> t

-- 3. and 4.
second xs = head (tail xs)
-- :type second
-- second :: [a] -> a

swap (x,y) = (y,x)
-- :type swap
-- swap :: (t1, t) -> (t, t1)

pair x y = (x,y)
-- :type pair
-- pair :: t1 -> t -> (t1, t)

double x = x*2
-- :type double
-- double :: Num a => a -> a

palindrome xs = reverse xs == xs
-- :type palindrome
-- palindrome :: Eq a => [a] -> Bool

twice f x = f (f x)
-- :type twice
-- twice :: (t -> t) -> t -> t

-- 5.
-- All possible answers have to be generated and compared
