module Chapter4 where

-- 1.
halve :: [a] -> ([a],[a])
halve xs = (take half xs, drop half xs)
           where half = length xs `div` 2

-- 2.
third :: [a] -> a
third xs = head (tail (tail xs))

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (_:_:thrd:_) = thrd

-- 3.
safetail :: [a] -> [a]
safetail xs = if null xs then xs else tail xs

safetail' :: [a] -> [a]
safetail' xs  | null xs = xs
              | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (x:xs) = xs

-- 4.
(||) :: Bool -> Bool -> Bool
True || True   = True
True || False  = True
False || True  = True
False || False = False

(|||) :: Bool -> Bool -> Bool
False ||| False = False
_     ||| _     = True

(||||) :: Bool -> Bool -> Bool
False |||| b   = b
True  |||| _   = True

(|||||) :: Bool -> Bool -> Bool
b ||||| c  | b == c = b
           | otherwise = True

-- 5.
(&&) :: Bool -> Bool -> Bool
x && y = if x == True then
           if y == True then True else False 
         else False

-- 6.
(&&&) :: Bool -> Bool -> Bool
x &&& y = if x == True then y else False

-- 7.
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x * y * z))

-- 8.
luhnDouble :: Int -> Int
luhnDouble i =  if res > 9 then res - 9 else res
                where res = i * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z =  if total `mod` 10 == 0 then True else False
                where total = (luhnDouble w) + x + (luhnDouble y) + z

