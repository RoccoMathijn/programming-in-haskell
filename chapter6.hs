module Chapter6 where
import Prelude hiding (take, sum, last, (!!), (^), and, elem, concat, merge, replicate)

-- 1.
fac 0 = 1
fac n | n > 0  =  n * fac (n-1)

-- 2.
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 3.
(^) :: Int -> Int -> Int
x ^ 0 = 1
x ^ n = x * (x ^ (n-1))

-- 4.
euclid :: Int -> Int -> Int
euclid m n | m == n = m
           | m < n = euclid m (n-m)
           | otherwise = euclid (m-n) n

-- 6.          
and :: [Bool] -> Bool
and [] = True
and (False:_) = False
and (_:xs) = and xs

concat :: [[a]] -> [a]
concat [] = []
concat ([]:xs) = concat xs 
concat ((x:xs):xss) = x : concat (xs : xss)

replicate :: Int -> a -> [a]
replicate 1 a = [a] 
replicate n a = a : replicate (n - 1) a

(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(_:xs) !! n = xs !! (n-1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem a (x:xs) | a == x = True
              | otherwise = elem a xs

-- 7.
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys
-- 8.
halve :: [a] -> ([a],[a])
halve xs = (take(h) xs, drop(h) xs)
           where
            h = length(xs) `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort( fst( halve xs))) (msort( snd( halve xs)) )

-- 9.
-- a.
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

-- b.
take :: Int -> [a] -> [a]
take _ [] = []
take 0 xs = [] 
take n (x:xs) = x : take (n - 1) xs

-- c.
last :: [a] -> a
last [x] = x
last (x:xs) = last xs
