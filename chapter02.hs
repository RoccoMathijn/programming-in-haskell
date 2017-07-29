module Chapter2 where
import Prelude hiding (last, init)

-- 2.
-- (2^3)*4
-- (2*3)+(4*5)
-- 2+(3*(4^5))

-- 3.
n = a `div` length xs
    where
      a = 10
      xs = [1,2,3,4,5]

-- 4.
last xs = xs !! (length xs - 1)
last' xs = head (reverse xs)

-- 5.
init xs = reverse (tail (reverse xs))
init' xs = take ((length xs) - 1) xs
