
puissance :: Integer -> Integer -> Integer
puissance a 0 = 1
puissance a n = if (n<0) then 0 else a * puissance a (n-1)

fact :: Integer -> Integer
fact 0 = 1
fact n = if (n<0) then 0 else n * fact (n-1)

squaresum :: Integer -> Integer
squaresum 0 = 0
squaresum n = if (n<0) then 0 else n * n + squaresum (n-1)

fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n = if (n<0) then 0 else (+) (fibo (n-1)) (fibo (n-2))

power :: Integer -> Integer -> Integer
power a 0 = 1
power a n = if (n<0) then 0 else ((\x -> x * x) (power a (div n 2))) * (if (mod n 2 == 0) then 1 else a)
