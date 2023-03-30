{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Zipper where

data Zipper a = Cell [a] [a] deriving (Show)

liste_en_zipper :: [a] -> Zipper a
liste_en_zipper = Cell []

zipper_en_liste :: Zipper a -> [a]
zipper_en_liste (Cell [] r) = r
zipper_en_liste (Cell (l:ls) r) = zipper_en_liste (Cell ls (l:r))

zipper_est_vide :: Zipper a -> Bool
zipper_est_vide (Cell [] []) = True
zipper_est_vide (Cell _ _) = False

est_au_debut :: Zipper a -> Bool
est_au_debut (Cell [] _) = True
est_au_debut (Cell _ _) = False

est_a_la_fin :: Zipper a -> Bool
est_a_la_fin (Cell _ []) = True
est_a_la_fin (Cell _ _) = False

recule :: Zipper a -> Zipper a
recule (Cell l@(x:xs) r) = Cell (init l) (last l:r) -- init donne la liste sans le dernier élément

avance :: Zipper a -> Zipper a
avance (Cell l (x:xs)) = Cell (l++[x]) xs

rembobine :: Zipper a -> Zipper a
rembobine (Cell [] r) = Cell [] r
rembobine x = rembobine (recule x)

insere_avant :: a -> Zipper a -> Zipper a
insere_avant x (Cell l r) = Cell (l++[x]) r

insere_apres :: a -> Zipper a -> Zipper a
insere_apres x (Cell l r) = Cell l (x:r)

element_precedent :: Zipper a -> a
element_precedent (Cell l r) = last l -- last donne le dernier élément d'une liste

element_suivant :: Zipper a -> a
element_suivant (Cell l (x:xs)) = x

enleve_precedent :: Zipper a -> Zipper a
enleve_precedent (Cell l r) = Cell (init l) r

enleve_suivant :: Zipper a -> Zipper a
enleve_suivant (Cell l (x:xs)) = Cell l xs

-- Comment vérifier que l'implémentation est correcte ?

-- On peut écrire des tests unitaires pour chaque fonction de notre code
-- Les tests unitaires consistent à tester chaque fonction individuellement avec différentes entrées pour garantir que la fonction retourne le résultat attendu

-- On peut effectuer des tests de propriétés pour vérifier si une propriété spécifique est respectée pour toutes les entrées possibles d'une fonction
-- Par exemple, on peut tester si la fonction zipper_en_liste inverse correctement la transformation effectuée par la fonction liste_en_zipper

-- On peut effectuer des tests de bord pour tester les limites de notre implémentation
-- Par exemple, on peut tester si les fonctions recule, avance, enleve_precedent et enleve_suivant fonctionnent correctement lorsqu'on atteint le début ou la fin de la liste

-- On peut effectuer des tests d'intégration pour tester l'interaction entre différentes parties de notre code
-- Par exemple, on peut tester l'ensemble des fonctions implémentées pour s'assurer qu'elles fonctionnent correctement ensemble