
myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x:myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f (x:xs) = if f x then x:myFilter f xs else myFilter f xs

myUnzip :: [(a,b)] -> ([a], [b])
myUnzip [] = ([],[])
myUnzip ((x,y):xs) = (x:fst a,y:snd a) where a = myUnzip xs
