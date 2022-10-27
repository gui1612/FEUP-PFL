-- 1.1

testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = (a + b < c) && (b < a + c) && (c < a + b)

-- 1.2

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt (s * (s - a) * (s - b) * (s - c))
    where s = (a + b + c) / 2

-- 1.3

metades :: [a] -> ([a], [a])
metades lst = (take half lst, drop half lst)
    where half = length lst `div` 2

-- 1.4

-- a)

last' :: [a] -> a
last' lst = head (reverse lst)

last'' :: [a] -> a
last'' lst = head (drop (length lst - 1) lst)

-- b)

init' :: [a] -> [a]
init' lst = take (length lst - 1) lst

init'' :: [a] -> [a]
init'' lst = reverse (drop 1 (reverse lst))

-- 1.5

-- a)

binom :: Integer -> Integer -> Integer
binom n k = product [1..n] `div` (product [1..k] * product [1..n-k])

-- b)

binom' :: Integer -> Integer -> Integer
binom' n k | k < (n - k)        = product [n-k+1..n] `div` product [1..k]
           | otherwise          = product [k+1..n] `div` product [1..n-k]
    
-- 1.6

raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c = ((-b + sqrt delta) / den, (-b + sqrt delta) / den)
    where delta = b^2 - 4*a*c
          den = 2*a

-- 1.7

{-
    a) [Char]
    b) (Char, Char, Char)
    c) [(Bool, Char)]
    d) ([Bool], [Char])
    e) [[a] -> [a]]
    f) [Bool -> Bool]
-}

-- 1.8

{-
    a) [a] -> a
    b) (a, b) -> (b, a)
    c) a -> b -> (a, b)
    d) Num a => a -> a
    e) Fractional a => a -> a
    f) Char -> Bool
    g) Ord a => a -> a -> a -> Bool
    h) Eq a => [a] -> Bool
    i) (a -> a) -> a -> a
-}

-- 1.9

classifica :: Int -> String
classifica grade | grade <= 9 && grade >= 0       = "reprovado"
                 | grade <=12                     = "suficiente"
                 | grade <=15                     = "bom"
                 | grade <=18                     = "muito bom"
                 | grade <=20                     = "muito bom com distincao"
                 | otherwise                      = "nota invalida"

-- 1.10

classifica' :: Float -> Float -> String
classifica' w h | imc < 18.5 && imc >= 0       = "baixo peso"
                | imc < 25                     = "peso normal"
                | imc < 30                     = "excesso de peso"
                | imc >= 30                    = "obesidade"
                | otherwise                    = "imc invalido"
                where imc = w / (h^2)

-- 1.11

-- a)

max3 :: Ord a => a -> a -> a -> a
max3 a b c = max a (max b c)

min3 :: Ord a => a -> a -> a -> a
min3 a b c = min a (min b c)


-- b)

max3' :: Ord a => a -> a -> a -> a
max3' a b c | a >= b && b >= c    = a
            | b >= a && a >= c    = b
            | otherwise           = c

min3' :: Ord a => a -> a -> a -> a
min3' a b c | a <= b && b <= c    = a
            | b <= a && a <= c    = b
            | otherwise           = c

-- 1.12

xor, xor' :: Bool -> Bool -> Bool
xor True False  = True
xor False True  = True
xor False False = False
xor True True   = False

xor' a b = a /= b

-- 1.13

safetail, safetail', safetail'' :: [a] -> [a]
safetail lst   = drop 1 lst
safetail' lst  = reverse (take (length lst - 1) (reverse lst))
safetail'' []  = [] 
safetail'' lst = tail lst

-- 1.14

curta, curta' :: [a] -> Bool
curta lst = length lst <= 2

curta' []       = True
curta' [_]      = True
curta' [_,_]    = True
curta' [_,_,_]  = True
curta' _         = False

-- 1.15

mediana :: Ord a => a -> a -> a -> a
mediana a b c | (a <= b && a >= c) || (b >= a && a <= c)    = a 
              | (b <= a && b >= c) || (a >= b && b <= c)    = b
              | otherwise                                   = c


mediana' :: (Num a, Ord a) => a -> a -> a -> a
mediana' a b c = a + b + c - max3 a b c - min3 a b c