{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

halve :: [Int] -> ([Int], [Int])
halve [] = ([], []) -- Cas où la liste est vide
halve [x] = ([x], []) -- Cas où la liste a un élément
halve (x:y:zs) = (x:xs, y:ys) -- Cas général, avec deux éléments à chaque étape
  where (xs, ys) = halve zs

combine :: [Int] -> [Int] -> [Int]
combine [] ys = ys -- Cas où la première liste est vide
combine xs [] = xs -- Cas où la deuxième liste est vide
combine (x:xs) (y:ys)
  | x <= y    = x : combine xs (y:ys) -- Cas où x est inférieur ou égal à y
  | otherwise = y : combine (x:xs) ys -- Cas où y est inférieur à x

tri_fusion :: [Int] -> [Int]
tri_fusion [] = [] -- Cas où la liste est vide
tri_fusion [x] = [x] -- Cas où la liste a un élément
tri_fusion xs = combine (tri_fusion ys) (tri_fusion zs) -- Cas général
  where (ys, zs) = halve xs
