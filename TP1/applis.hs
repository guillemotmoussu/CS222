-- >= a deux arguments

z = (>=) 3
-- z évalue si le nombre passé en argument est >=3

-- map z [1..5] renvoie un tableau de 5 éléments (Booléens) dont chaque élément est l'application de z à 1, puis 2, ... jusqu'à 5

app f = map f [1..5]

-- le type de app est (a -> b) -> [b]
-- le type de app z est [Bool]

-- en donnant app z on obtient le même résultat qu'à la question 2 (i.e. map z [1..5])

-- app succ calcule le successeur de tous les élements de la liste

max4 x = if x>=4 then x else 4
-- on a bien [4,4,4,4,5]
-- on peut aussi utiliser app (max 4)

echange f a b = f b a
mod2 = echange mod 2