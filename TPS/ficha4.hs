data Arv a = Vazia | No a (Arv a) (Arv a)

-- 4.1

sumArv :: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (No n l r) = n + sumArv l + sumArv r

-- 4.2

listar :: Arv a -> [a]
listar Vazia = []
listar (No n l r) = listar r ++ [n] ++ listar l

-- 4.3

nivel :: Int -> Arv a -> [a]
nivel _ Vazia = []
nivel 0 (No n _ _) = [n]
nivel nv (No n l r) = nivel (nv - 1) l ++ nivel (nv - 1) r

-- 4.4

construir :: [a] -> Arv a
construir [] = Vazia
construir xs = No x (construir esq) (construir dir)
    where mid = length xs `div` 2
          esq = take mid xs
          x:dir = drop mid xs


-- 4.5

mapArv :: (a -> b) -> Arv a -> Arv b
mapArv _ Vazia = Vazia
mapArv f (No n l r) = No (f n) (mapArv f l) (mapArv f r)

-- 4.6

-- a)

maisEsq :: Arv a -> a
maisEsq (No n Vazia _) = n
maisEsq (No _ l _) = maisEsq l

maisDir :: Arv a -> a
maisDir (No n _ Vazia) = n
maisDir (No _ _ r) = maisDir r

-- b)



remover :: Ord a => a -> Arv a -> Arv a
remover _ Vazia = Vazia
remover x (No n l Vazia) 
    | x == n   = l
remover x (No n Vazia r) 
    | x == n   = r
remover x (No n l r) 
    | x < n         = No n (remover n l) r
    | x > n         = No n l (remover n r)
    | otherwise     = let val = maisDir l
                      in  No val (remover val l) r

-- 4.7

reverseStr :: String -> String 
reverseStr [] = []
reverseStr (x:xs) = reverseStr xs ++ [x]

getLines :: IO [String]
getLines = do
    line <- getLine
    if line == ""
        then return []
    else
        do
            rest <- getLines
            return (line:rest)

reverseLines :: IO()
reverseLines = do
    lines <- getLines
    mapM_ (putStrLn . reverseStr) lines

    