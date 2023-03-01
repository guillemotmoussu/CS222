
z :: Integer -> Bool
z = (>=) 3

app :: (Integer -> a) -> [a]
app f = map f [1..5]

echange :: (a -> b -> c) -> (b -> a -> c)
echange f x y = f y x

mod2 :: Integer -> Integer
mod2 = echange mod 2
