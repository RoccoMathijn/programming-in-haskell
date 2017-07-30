module Chapter15 where
import Prelude hiding (repeat, take, replicate)

-- 4.
fibs :: [Integer]
fibs = 0 : 1 : [prev + prevprev | (prevprev, prev) <- zip fibs (tail fibs)]

-- 5.
--data Tree a = Leaf | Node (Tree a) a (Tree a)
--              deriving Show

-- this is all wrong
--repeat :: Tree a -> Tree a
--repeat (Node l v r) = repeat (Node (Node l v r) v (Node l v r))

--take :: Int -> Tree a -> Tree a
--take 0 _ = Leaf
--take _ Leaf = Leaf
--take n (Node l v r) = Node (take (n-1) l) v (take (n-1) r)

--replicate :: Int -> Tree a -> Tree a
--replicate n = take n . repeat

-- 6.
sqroot' :: Double -> Double
sqroot' n = snd (head (filter (\t -> (snd t) - (fst t) < 0.00001) zipped)) 
            where
            a       = [a | a <- iterate (\a -> (a + n / a) / 2.0) 1.0]
            zipped  = zip a (tail a)
