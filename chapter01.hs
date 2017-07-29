module Chapter1 where
import Prelude hiding (product)

-- Solutions
--
-- 1.
-- double (double 2)
-- double (2 + 2)
-- double (4)
-- 4 + 4
-- 8

-- 2.
-- sum [x]
-- x + sum []
-- x + 0
-- x

-- 3.
product [] = 1
product (x:xs) = x * product xs 
-- product [2,3,4] = 24

-- 4.
qsort [] = []
qsort (x:xs) =  qsort larger ++ [x] ++ qsort smaller
                where 
                  smaller = [a | a <- xs, a <= x]
                  larger  = [b | b <- xs, b > x]

-- 5.
-- Duplicate values will be lost
qsort' [] = []
qsort' (x:xs) =  qsort' smaller ++ [x] ++ qsort' larger
                 where
                   smaller = [a | a <- xs, a < x]
                   larger  = [b | b <- xs, b > x]

-- qsort' [2,2,3,1,1] = [1,2,3]
