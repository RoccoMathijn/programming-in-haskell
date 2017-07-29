module Chapter7 where
import Prelude hiding (all, any, takeWhile, dropWhile, map, filter, curry, uncurry)
import Data.Char

-- 1.
exercise1 f p xs = map f (filter p xs)

-- 2.
-- a.
all :: (a -> Bool) -> [a] -> Bool
all p xs = foldr (\x acc -> p x && acc) True xs

all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p 

-- b.
any :: (a -> Bool) -> [a] -> Bool
any p = or . map p

-- c.
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []                  = []
takeWhile p (x:xs) | p x        = x : takeWhile p xs
                   | otherwise  = [] 

-- d.
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []                  = []
dropWhile p (x:xs) | p x        = dropWhile p xs 
                   | otherwise  = x:xs 

-- 3.
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x acc -> f x : acc) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x acc -> if (p x) then x : acc else acc) []

-- 4.
dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> acc * 10 + x) 0

-- 5.
curry :: ((a, b) -> c) -> (a -> b -> c)
curry f x y = f (x, y)  

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (x, y) = f x y  

-- 6.
unfold p h t x  | p x = []
                | otherwise = h x : unfold p h t (t x)

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold (==[]) (take 8) (drop 8)

map' :: (a -> b) -> [a] -> [b]
map' f = unfold (null) (f.head) (tail)

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\x -> False) (f) (f)

-- 7.
-- to be continued
type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x acc -> x + 2 * acc) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

addParity :: [Bit] -> [Bit]
addParity bits =  parityBit : bits
                  where parityBit = if oddOnes bits then 1 else 0

oddOnes :: [Bit] -> Bool
oddOnes bits = length ones `mod` 2 /= 0
             where ones = filter (==1) bits

encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

checkParity :: [Bit] -> [Bit]
checkParity (parityBit:xs) = case (oddOnes xs, parityBit) of
                               (True, 1)  -> xs 
                               (False, 0) -> xs
                               (_, _)     -> error "Your data was corrupted"  

decode :: [Bit] -> String
decode bits = map (chr . bin2int) (map checkParity (chop9 bits))

transmit :: String -> String
transmit = decode . corruptChannel . encode

channel :: [Bit] -> [Bit]
channel = id

corruptChannel :: [Bit] -> [Bit]
corruptChannel = tail
