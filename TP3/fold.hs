
sommeListe :: [Int] -> Int
sommeListe [] = 0
sommeListe (x:xs) = x + sommeListe xs

accInts :: (Int -> Int -> Int) -> Int -> [Int] -> Int
accInts f i [] = i
accInts f i (x:xs) = f x (accInts f i xs)

-- Les deux fonctions ci-dessus calculent de droite à gauche

maxListe :: [Int] -> Int
maxListe = accInts max 0

--GHCI propose acc :: (t1 -> t2 -> t2) -> t2 -> [t1] -> t2
acc f i [] = i
acc f i (x:xs) = f x (acc f i xs)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f i [] = i
myFoldl f i (x:xs) = f (myFoldl f i xs) x

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f i [] = i
myFoldr f i (x:xs) = f x (myFoldr f i xs)

join :: [[Char]] -> [Char]
join [] = []
join [x] = x
join (x:xs) = x ++ ' ':join xs
