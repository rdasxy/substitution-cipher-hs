import qualified Data.Map as Map

{--
--My indexof version
indexof y [] = -1
indexof y xs = index y xs 0
index y (x:xs) n 
    | y == x then n
    | otherwise index y xs (n+1)
--}

--Saul's indexof version 1
indexof :: (Eq a) => a -> [a] -> Int
indexof y [] = -1
indexof y (x:xs)
  | not (y `elem` (x:xs)) = -1
  | y == x = 0
  | otherwise = 1 + indexof y xs

--Saul's indexof version 2
indexof2 :: (Eq a) => a -> [a] -> Int
indexof2 y ys 
  | not (y `elem` ys) = -1
  | y == head ys = 0
  | otherwise = 1 + indexof2 y (tail ys)

--Matches 
match :: (Eq a) => [a] -> [a] -> Bool  
match [] [] = True
match _ [] = False
match [] _ = False
match (p:ps) (c:cs) = match ps cs && (indexof2 p ps == indexof2 c cs)

--Gives Ciphers
let m = Maps.fromList (zip ps cs)

newCipher = 
  let x = A union B in
    if x == B union A
    then x
    else Nothing

--Cartesian Product
cartesian = foldr [[]] where f l a = [x: xs | x <- l, xs <- a]
