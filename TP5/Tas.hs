
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

data Tas a = Noeud Int a (Tas a) (Tas a) | Vide deriving (Show)

t1 :: Tas Int
t1 = Noeud 8 (-8)
    (Noeud 3 1 (Noeud 1 5 Vide Vide) (Noeud 1 42 Vide Vide))
    (Noeud 4 (-3) (Noeud 2 28 Vide (Noeud 1 60 Vide Vide)) (Noeud 1 7 Vide Vide))

taille :: Tas a -> Int
taille Vide = 0
taille (Noeud x _ _ _) = x

tas_min :: Tas a -> a
tas_min (Noeud _ x _ _) = x

noeud :: Ord a => a -> Tas a -> Tas a -> Tas a
noeud t x y | (&&) (t <= tas_min x) (t <= tas_min y) = Noeud (taille x + taille y + 1) t x y

est_equilibre :: Tas a -> Bool
est_equilibre Vide = True
est_equilibre (Noeud _ _ x y) = (&&)
    ((taille x - taille y) < 2 && (taille y - taille x) < 2)
    (est_equilibre x && est_equilibre y)

ajouter :: Ord a => a -> Tas a -> Tas a
ajouter t Vide = noeud t Vide Vide
ajouter t (Noeud _ v x y)
    | (&&) (t > v) (taille x > taille y) = ajouter t y
    | t > v = ajouter t x
    | taille x > taille y = noeud t x (ajouter (tas_min y) y)
    | otherwise = noeud t (ajouter (tas_min x) x) y

--retirer_feuille :: Tas a -> (a, Tas a)
--retirer_feuille (Noeud 1 x Vide Vide) = x, (Noeud 1 Vide Vide)
--retirer_feuille (Noeud _ _ x y) = max(retirer_feuille x,retirer_feuille y),?
