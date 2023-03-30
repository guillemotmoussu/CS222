-- Test de la création d'un zipper à partir d'une liste
let z = liste_en_zipper [1, 2, 3, 4]

-- Test de la conversion d'un zipper en liste
zipper_en_liste z -- doit retourner [1, 2, 3, 4]

-- Test de la fonction qui vérifie si un zipper est vide
zipper_est_vide z -- doit retourner False

-- Test de la fonction qui vérifie si le curseur est au début du zipper
est_au_debut z -- doit retourner True

-- Test de la fonction qui vérifie si le curseur est à la fin du zipper
est_a_la_fin z -- doit retourner False

-- Test de la fonction qui fait reculer le curseur
let z' = recule z
zipper_en_liste z' -- doit retourner [2, 3, 4, 1]
est_au_debut z' -- doit retourner False

-- Test de la fonction qui fait avancer le curseur
let z'' = avance z'
zipper_en_liste z'' -- doit retourner [1, 2, 3, 4]
est_a_la_fin z'' -- doit retourner False

-- Test de la fonction qui rembobine le zipper au début
let z''' = rembobine z''
zipper_en_liste z''' -- doit retourner [1, 2, 3, 4]
est_au_debut z''' -- doit retourner True

-- Test de la fonction qui insère un élément avant le curseur
let z1 = insere_avant 0 z
zipper_en_liste z1 -- doit retourner [0, 1, 2, 3, 4]
est_au_debut z1 -- doit retourner True

-- Test de la fonction qui insère un élément après le curseur
let z2 = insere_apres 5 z1
zipper_en_liste z2 -- doit retourner [0, 1, 2, 3, 4, 5]
est_a_la_fin z2 -- doit retourner True

-- Test de la fonction qui retourne l'élément précédent
element_precedent z2 -- doit retourner 4

-- Test de la fonction qui retourne l'élément suivant
element_suivant z2 -- doit retourner 5

-- Test de la fonction qui enlève l'élément précédent
let z3 = enleve_precedent z2
zipper_en_liste z3 -- doit retourner [0, 1, 2, 3, 5]
element_precedent z3 -- doit retourner 3

-- Test de la fonction qui enlève l'élément suivant
let z4 = enleve_suivant z3
zipper_en_liste z4 -- doit retourner [0, 1, 2, 3]
element_suivant z4 -- doit retourner 3
