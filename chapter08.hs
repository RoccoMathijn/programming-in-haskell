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
data BTree a = BLeaf a | BNode (BTree a) (BTree a)

leaves :: BTree a -> Int
leaves (BLeaf _)    = 1
leaves (BNode l r)  = (leaves l) + (leaves r)

balanced :: BTree a -> Bool
balanced (BLeaf y)   = True
balanced (BNode l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r

halves :: [a] -> ([a],[a])
halves list = (take half list, drop half list)
              where half = (length list `div` 2)

-- 4.
balance :: [a] -> BTree a
balance [x] = BLeaf x
balance xs = BNode (balance (fst (halves xs))) (balance (snd (halves xs)))

-- 5.
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g e = case e of
              Val x   -> f x
              Add x y -> g (folde f g x) (folde f g y)

-- 6.
eval' :: Expr -> Int
eval' e = folde (\x -> x) (\x y -> x + y) e

-- 7.
--instance Eq a => Eq (Maybe a) where
--  Just x    == Just y     = x == y
--  Nothing   == Nothing    = True
--  _         == _          = False

--instance Eq a => Eq [a] where
--  [] == [] = True
--  (x:xs) == (y:ys) = x == y && xs == ys
--  _ == _ = False

-- 8.

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Disj Prop Prop
          | Eq Prop Prop

type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

eval :: Subst -> Prop -> Bool
eval _ (Const b)    = b
eval s (Var x)      = find x s
eval s (Not p)      = not (eval s p)
eval s (And p q)    = eval s p && eval s q
eval s (Imply p q)  = eval s p <= eval s q
eval s (Disj p q)   = not (eval s p) && not (eval s q)
eval s (Eq p q)     = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

subst :: Prop -> [Subst]
subst p = map (zip vs) (bools (length vs))
          where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- subst p]
