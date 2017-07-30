module Chapter8 where

-- 1.
data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero      n = n
add (Succ m)  n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _         = Zero
mult _    Zero      = Zero
mult m    (Succ n)  = add m (mult m n)

-- 2.
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)      = x == y
occurs x (Node l y r)  = case compare x y of
                          LT -> occurs x l
                          EQ -> True
                          GT -> occurs x r
-- The new definition only requires one comparison of x and y per node

-- 3.
data BTree a = Leaf a | Node (BTree a) (BTree a)

leaves :: BTree a -> Int
leaves (Leaf _)    = 1
leaves (Node l r)  = (count l) + (count r)

balanced :: BTree a -> Bool
balanced (Leaf y)   = True
balanced (Node l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

halves :: [a] -> ([a],[a])
halves list = (take half list, drop half list)
              where half = (length list `div` 2)

-- 4.
balance :: [a] -> BTree a
balance [x] = Leaf x
balance xs = Node (balance (fst (halves xs))) (balance (snd (halves xs)))

-- 5.
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g e = case e of
              Val x   -> f x
              Add x y -> g (folde f g x) (folde f g y)

-- 6.
eval :: Expr -> Int
eval e = folde (\x -> x) (\x y -> x + y) e

-- 7.
instance Eq a => Eq (Maybe a) where
Just x    == Just y     = x == y
Nothing   == Nothing    = True
_         == _          = False

instance Eq a => Eq [a] where
[] == [] = True
(x:xs) == (y:ys) = x == y && xs == ys
_ == _ = False
