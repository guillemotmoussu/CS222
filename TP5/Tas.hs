
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

data Tas a = Noeud Int a (Tas a) (Tas a) | Vide deriving (Show)

t1 :: Tas Int
t1 = Noeud 8 (-8)
    (Noeud 3 1 (Noeud 1 5 Vide Vide) (Noeud 1 42 Vide Vide))
    (Noeud 4 (-3) (Noeud 2 28 Vide (Noeud 1 60 Vide Vide)) (Noeud 1 7 Vide Vide))

taille :: Tas a -> Int
taille Vide = 0
taille (Noeud t _ _ _) = t

tas_min :: Tas a -> a
tas_min (Noeud _ v _ _) = v

noeud :: Ord a => a -> Tas a -> Tas a -> Tas a
noeud t Vide Vide = Noeud 1 t Vide Vide
noeud t Vide y | t <= tas_min y = Noeud (0 + taille y + 1) t Vide y
noeud t x Vide | t <= tas_min x = Noeud (taille x + 0 + 1) t x Vide
noeud t x y = Noeud (taille x + taille y + 1) t x y

est_equilibre :: Tas a -> Bool
est_equilibre Vide = True
est_equilibre (Noeud _ _ x y) = (&&)
    (((taille x - taille y) < 2) && ((taille y - taille x) < 2))
    (est_equilibre x && est_equilibre y)

ajouter :: Ord a => a -> Tas a -> Tas a
ajouter t Vide = noeud t Vide Vide
ajouter t (Noeud _ v x y)
    | (t > v) && (taille x > taille y) = noeud v x (ajouter t y)
    | t > v = noeud v (ajouter t x) y
    | taille x > taille y = noeud t x (ajouter v y)
    | otherwise = noeud t (ajouter v x) y

retirer_feuille :: Ord a => Tas a -> (a, Tas a)
retirer_feuille (Noeud _ v Vide Vide) = (v, Vide)
retirer_feuille (Noeud _ v l r)
  | taille l > taille r = let (y, l') = retirer_feuille l in (y, noeud v l' r)
  | otherwise = let (y, r') = retirer_feuille r in (y, noeud v l r')

equilibrer :: Ord a => Tas a -> Tas a
equilibrer Vide = Vide
equilibrer (Noeud _ v l r)
  | (&&) (taille r < taille l + 2) (taille l < taille r + 2) = noeud v l r
  | taille l > taille r + 1 = let (x, l') = retirer_feuille l in equilibrer (noeud v (equilibrer l') (ajouter x (equilibrer r)))
  | taille r > taille l + 1 = let (y, r') = retirer_feuille r in equilibrer (noeud v (ajouter y (equilibrer l)) (equilibrer r'))

retirer :: Ord a => Tas a -> Tas a
retirer (Noeud _ _ Vide Vide) = Vide
retirer (Noeud _ _ x Vide) = x
retirer (Noeud _ _ Vide y) = y
retirer (Noeud _ _ x@(Noeud _ vx _ _) y@(Noeud _ vy _ _))
  | vx < vy = noeud vx (retirer x) y
  | otherwise = noeud vy x (retirer y)

construit :: Ord a => [a] -> Tas a
construit = foldr ajouter Vide

deconstruit :: Ord a => Tas a -> [a]
deconstruit Vide = []
deconstruit t = tas_min t:deconstruit (retirer t)

tri :: Ord a => [a] -> [a]
tri = deconstruit . construit
