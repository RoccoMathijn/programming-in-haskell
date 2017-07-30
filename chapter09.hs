module Chapter9 where

data Op = Add | Sub | Mul | Div | Pow

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "^"

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n)      = show n
  show (App o l r)  = brak l ++ show o ++ brak r
                      where
                      brak (Val n) = show n
                      brak e = "(" ++ show e ++")"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /=0 && y /= 1 && x `mod` y == 0
valid Pow x y = x <= y && y >= 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
--apply Pow x y = x ^ y

values :: Expr -> [Int]
values (Val n) = [n]
values (App o l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs)   = yss ++ map (x:) yss
                where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []       = [[x]]
interleave x (y:ys)   = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []  = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- 1.
choices' :: [a] -> [[a]]
choices' xs = [zs | ys <- subs xs, zs <- perms ys]

-- 2.
isChoice :: Eq a => [a] -> [a] -> Bool

isChoice (x:xs) (y:ys)  = elem x ys && isChoice xs (removeFirst x ys)

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst x (y:ys)  | x == y = ys
                      | otherwise = y : removeFirst x ys

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div] -- Pow]
-- 3.
-- solutions won't terminate

-- 4.
possibleExpr :: [Int] -> [Expr]
possibleExpr = concat . map exprs . choices

numberOfPossibleExpressions :: [Int] -> Int
numberOfPossibleExpressions = length . possibleExpr
--numberOfPossibleExpressions [1,3,7,10,25,50]
--33665406

--valid :: Op -> Int -> Int -> Bool
--valid Add _ _ = True
--valid Sub x y = x > y
--valid Mul _ _ = True
--valid Div x y = x `mod` y == 0

successfullExpr :: [Int] -> [[Int]]
successfullExpr = filter (not . null) . map eval . possibleExpr

numberOfSuccessfullExpressions :: [Int] -> Int
numberOfSuccessfullExpressions = length . successfullExpr 
--numberOfSuccessfullExpressions [1,3,7,10,25,50]
--4672540

-- 5.
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = True 
valid Mul _ _ = True
valid Div x y = y /= 0 && x `mod` y == 0

--numberOfSuccessfullExpressions [1,3,7,10,25,50]
--10839369

-- 6.
-- a.
--data Op = Add | Sub | Mul | Div | Pow
--instance Show Op where
--  show Add = "+"
--  show Sub = "-"
--  show Mul = "*"
--  show Div = "/"
--  show Pow = "^"

--valid :: Op -> Int -> Int -> Bool
--valid Add x y = x <= y
--valid Sub x y = x > y
--valid Mul x y = x /= 1 && y /= 1 && x <= y
--valid Div x y = y /=0 && y /= 1 && x `mod` y == 0
--valid Pow x y = x <= y && y >= 0

--ops = [Add, Sub, Mul, Div] -- Pow]

-- b.
-- c.
