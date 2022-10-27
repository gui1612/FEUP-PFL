import Data.List
import Data.Char

-- 2.1

-- a)

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

-- b)

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

-- c)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- d)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = [x] ++ replicate' (n-1) x

-- e) 

(?!) :: [a] -> Int -> a
(x:_) ?! 0 = x
(_:xs) ?! n = xs ?! (n-1)

-- f)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs) = (n == x) || elem' n xs

-- 2.2

intersperse' :: a -> [a] -> [a]
intersperse' _ [a] = [a]
intersperse' a (x:xs) = x:a:intersperse' a xs

-- 2.3

mdc, mdc' :: Integer -> Integer -> Integer
mdc a b | b == 0        = a
        | otherwise     = mdc b (mod a b)

mdc' a 0 = a
mdc' a b = mdc b (mod a b)


-- 2.4

-- a)

insert' :: Ord a  => a -> [a] -> [a]
insert' a [] = [a]
insert' a (x:xs) | a > x        = x:insert' a xs
                 | otherwise    = a:x:xs

-- b)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert' x (isort xs)


-- 2.5

-- a)

minimum' :: Ord a => [a] -> a
minimum' [a] = a
minimum' (x:xs) | x < minVal   = x
                | otherwise    = minVal
                where minVal = minimum' xs

-- b)

delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' a (x:xs) = if a == x then xs else x:delete' a xs

-- c)

ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort lst = minVal:ssort (delete' minVal lst)
    where minVal = minimum' lst

-- 2.6

somaComp :: Integral a => a -> a -> a
somaComp a b = sum [x^2 | x <- [a..b]]

-- 2.7

-- a)

aprox :: Int -> Double
aprox n = sum [(-1)^k / fromIntegral (2*k+1) | k <- [0..n]]

-- b)

aprox' :: Int -> Double
aprox' n = sum (take n [(-1)^k/ fromIntegral ((k+1)^2) | k <- [0..]])

-- 2.8

dotprod, dotprod' :: [Float] -> [Float] -> Float
dotprod x y = sum [a * b | (a, b) <- zip x y]

dotprod' [] [] = 0
dotprod' (x:xs) (y:ys) = x*y + dotprod' xs ys


-- 2.9

divprop :: Integer -> [Integer]
divprop n = [x | x <- [1..n-1], n `mod` x == 0]

-- 2.10

perfeitos :: Integer -> [Integer]
perfeitos n = [p | p <- [1..n], p == sum (divprop p)]

-- 2.11

pitagoricos :: Integer -> [(Integer, Integer, Integer)]
pitagoricos n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 2.12

primo :: Integer -> Bool
primo n = length (divprop n) == 1

-- 2.13

isMersennes :: Integer -> Bool
isMersennes n = primo n && not (null ([j | j <- [1..n], (2^j-1) == n]))

mersennes :: [Integer]
mersennes = [m | m <- [1..30], primo m && not (null ([j | j <- [0..30], (2^j-1) == m]))]

-- 2.14

fact :: Integer -> Integer
fact n = product [1..n]


binom :: Integer -> Integer -> Integer
binom n p = fact n `div` (fact p * fact (n-p))

pascal :: Integer -> [[Integer]]
pascal n = [[binom i j | j <- [0..i]] | i <- [0..n]]

-- 2.15

cifraLetra :: Int -> Char -> Char
cifraLetra n c | isLetter c && isUpper c = chr ((ord c + n - ord 'A') `mod` 26 + ord 'A')
               | isLetter c && isLower c = chr ((ord c + n - ord 'a') `mod` 26 + ord 'a')     
               | otherwise               = c

cifrar :: Int -> String -> String
cifrar n xs = [ cifraLetra n c | c <- xs]

-- 2.16

concat'' :: [[a]] -> [a]
concat'' lst = [a | subLst <- lst, a <- subLst]

replicate'' :: Int -> a -> [a]
replicate'' n e = [e | _ <- [1..n]]

excExc :: [a] -> Int -> a
excExc xs n = head [x | (x, y) <- zip xs [0..n], y == n]

-- 2.17

forte :: String -> Bool
forte s = length s >= 8 && hasUpperCase && hasLowerCase && hasAlg
    where hasUpperCase = not ( null [b | b <- s, isUpper b])
          hasLowerCase = not ( null [b | b <- s, isLower b])
          hasAlg = not ( null [b | b <- s, isDigit b])

-- 2.18

-- a)

mindiv :: Int -> Int
mindiv n | null divs = n
         | otherwise = head divs
         where divs = [x | x <- [2.. floor (sqrt (fromIntegral n))], n `mod` x == 0]

-- b)
primo' :: Int -> Bool
primo' n | n <= 1       = False
         | otherwise    = (mindiv n) == n 

-- 2.19

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x:nub [s | s <- xs, s /= x]

-- 2.20

transpose' :: [[a]] -> [[a]]
-- transpose' m = [[x, y] | (x, y) <- zip |]
transpose' m = [[r !! c | r <- m] | c <- [0..rowLen-1]]
    where rowLen = length (head m)


-- 2.21

algarismosAux :: Int -> [Int]
algarismosAux n | n < 10    = [n]
                | otherwise = [n `mod` 10] ++ algarismosAux (n `div` 10)

algarismos :: Int -> [Int]
algarismos n = reverse (algarismosAux n)

-- 2.22

toBits :: Int -> [Int]
toBits n        | n < 2    = [n]
                | otherwise = toBits (n `div` 2) ++ [n `mod` 2]

-- 2.23

fromBits :: [Int] -> Int
fromBits b | length b == 1      = head b
           | otherwise          = sum [2^n * b !! (len - n - 1) | n <- [0..len]]
           where len = length b


-- 2.24

-- a)

merge' :: Ord a => [a] -> [a] -> [a]
merge' [] a = a
merge' a [] = a
merge' (x:xs) (y:ys) | x < y        = x:merge' xs (y:ys)
                     | otherwise    = y:merge' (x:xs) ys

-- b)

metades :: [a] -> ([a], [a])
metades xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge' (msort l) (msort r)
        where (l,r) = metades xs
