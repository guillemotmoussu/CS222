
data BinTree a =                    -- Type somme
    Leaf a |                        -- Une feuille contient seulement sa valeur
    Node a (BinTree a) (BinTree a)  -- Un noeud contient sa valeur, et 2 sous-arbres
    deriving Show                   -- Permet l’affichage des valeurs par l’interprète

-- a représente le type des éléments de l'arbre
-- Leaf correspond aux feuilles, et Node correspond aux noeuds
-- (Bintree a) et (Bintree a) correspondent aux deux enfants des noeuds internes

arbre_exemple :: BinTree Int
arbre_exemple = Node (-8) (Node 18 (Leaf (-5)) (Leaf 42)) (Node 13 (Node 28 (Leaf 4) (Leaf 0)) (Leaf 73))

nombre_feuilles :: BinTree a -> Int
nombre_feuilles (Node _ x y) = (nombre_feuilles x) + (nombre_feuilles y)
nombre_feuilles (Leaf _) = 1

hauteur :: BinTree a -> Int
hauteur (Node _ x y) = 1 + max (hauteur x) (hauteur y)
hauteur (Leaf _) = 1

abr_exemple :: BinTree Integer
abr_exemple = Node 4 (Node (-5) (Leaf (-8)) (Leaf 0)) (Node 18 (Leaf 13) (Node 42 (Leaf 28) (Leaf 73)))

rechercher_abr :: BinTree Integer -> Integer -> Bool
rechercher_abr (Leaf x) n = (n == x)
rechercher_abr (Node x y z) n
                |(n == x) = True
                |otherwise = (rechercher_abr (if (n < x) then y else z) n)

aplatir_abr :: BinTree a -> [a]
aplatir_abr (Leaf x) = [x]
aplatir_abr (Node x y z) = aplatir_abr y ++ [x] ++ aplatir_abr z

-- Les deux nécessitent de trier les données au préalable (trier l'arbre en abr, ou la liste en liste triée)
-- En général, une recherche dichotomique sera plus rapide (log (n))
-- Plus la hauteur de l'arbre est imporante plus la recherche sera longue...
-- Car au lieu de couper au milieu on laissera l'un des côtés plus grand que l'autre
