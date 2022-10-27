-- 3.1

mapAndFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapAndFilter f p x = ((map f) . (filter p)) x

-- 3.2

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> x*10 + y) 0

-- 3.3

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- 3.4

insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs)   | n < x       = n:x:xs
                  | otherwise   = x:insert n xs 

-- 3.4

isort :: Ord a => [a] -> [a]
isort = foldr insert [] 

-- 3.5

-- a)

maximum', minimum' :: Ord a => [a] -> a
maximum' = foldr1 (\x y -> if x > y then x else y)
minimum' = foldr1 (\x y -> if x < y then x else y)

-- b)

foldl1', foldr1' :: (a -> a -> a) -> [a] -> a
foldl1' f (x:xs) = foldl f x xs
foldr1' f xs = foldl f (last xs) (init xs)

-- 3.6
mdc :: Int -> Int -> Int
mdc a b = fst (until (\(a,b) -> b == 0) (\(a,b) -> (b, a`mod`b))  (a,b))

-- 3.7

-- a)

(+++) :: [a] -> [a] -> [a]
xs +++ ys = foldr (:) ys xs

-- b)

concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- c)

reverse' :: [a] -> [a]
reverse' = foldr (\x xs -> xs ++ [x]) []

-- d)

reverse'' :: [a] -> [a]
reverse'' = foldl (\xs x -> x:xs) []

-- e)

elem' :: Eq a => a -> [a] -> Bool
elem' x = any (x==)

-- 3.8

-- a)

palavras :: String -> [String]
palavras [] = []
palavras xs =
    where rest = dropWhile (/= ' ') xs