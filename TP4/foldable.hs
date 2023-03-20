{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

data Bintree a = 
    Leaf a|
    Node a (Bintree a) (Bintree a)
    deriving Show

parcours_infixe :: Bintree a -> [a]
parcours_infixe (Leaf x) = [x]
parcours_infixe (Node x y z) = parcours_infixe y ++ [x] ++ parcours_infixe z

foldr_arbre :: (a -> b -> b) -> b -> Bintree a -> b
foldr_arbre f e (Leaf x) = f x e
foldr_arbre f e (Node x y z) = foldr_arbre f (f x (foldr_arbre f e z)) y

-- Jusqu'à maintenant, les 2 "valeurs" de t qu'on a utilisé sont Leaf et Node

to_list :: Foldable t => t a -> [a]
to_list = foldr (:) []

instance Foldable Bintree where
    foldr = foldr_arbre


-- sum :: (Foldable t, Num a) => t a -> a

-- Sur une structure accumulable (de la classe Foldable)
-- Contenant des éléments de Type numérique (de la classe Num)
-- La fonction sum va accumuler les éléments de la structure en utilisant:
--  - foldr (de la classe Foldable)
--  - (+) (de la classe Num)
-- pour calculer la somme des éléments contenus dans la structure
