module Chapter5 where
import Data.Char

-- 1.
sumIntegerSquares = sum [x^2 | x <- [1..100]]

-- 2.
grid :: Int -> Int -> [(Int, Int)]
grid x y = [(x',y') | x' <- [0..x] , y' <- [0..y]]

-- 3.
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 4.
replicate :: Int -> a -> [a]
replicate n t = [t | _ <- [1..n]]

-- 5.
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 6.
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfect :: Int -> [Int]
perfect n = [x | x <- [1..n], (sum (factors x)) - x  == x]

-- 7.
exercise7 = concat [[(x,y) | x <- [1,2]] | y <- [3,4]]

-- 8.
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = [i | i <- find x (zip xs [0..])]

-- 9.
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

-- 10.
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = toUpper (int2let ((let2int (toLower c) + n) `mod` 26))
                    | otherwise = c
lowers :: String -> Int
lowers xs = length [x | x <- xs, x>= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x | x' <- xs , x' == x]

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs =  [percent (count x xs) n | x <- ['a' .. 'z']]
            where n = lowers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

crack :: String -> String
crack xs = encode (-factor) xs
           where
             factor = head (positions (minimum chitab) chitab)
             chitab = [chisqr (rotate n table') table | n <- [0..25]]
             table' = freqs xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']
