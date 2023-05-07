
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use isNothing" #-}

planetes :: [(Double, String)]
planetes = [
    (0.39, "Mercure"),
    (0.72, "Venus"),
    (1.00, "Terre"),
    (1.52, "Mars"),
    (5.20, "Jupiter"),
    (9.54, "Saturne"),
    (19.19, "Uranus"),
    (30.06, "Neptune")
    ]

search :: Eq b => [(a,b)] -> b -> Maybe a
search [] _ = Nothing
search ((d,p):ls) x | x == p = Just d | otherwise = search ls x

distance :: String -> Maybe Double
distance = search planetes

search2 :: Ord a => [(a,b)] -> a -> Maybe b
search2 [] _ = Nothing
search2 ((d,p):ls) x | x < d = Just p | otherwise = search2 ls x

suivante :: Double -> Maybe String
suivante = search2 planetes

ua_vers_km :: Double -> Double
ua_vers_km = (*) 149597870

distance_km_1 :: String -> Maybe Double
distance_km_1 x | distance x == Nothing = Nothing
distance_km_1 x = let (Just y) = distance x in Just (ua_vers_km y)

fmap_Maybe :: (a -> b) -> Maybe a -> Maybe b
fmap_Maybe _ Nothing = Nothing
fmap_Maybe f (Just x) = Just (f x)

distance_km_2 :: String -> Maybe Double
distance_km_2 x = ua_vers_km <$> distance x

distance_suivante_1 :: String -> Maybe Double
distance_suivante_1 x | distance x == Nothing = Nothing
distance_suivante_1 x | let (Just y) = distance x in suivante y == Nothing = Nothing
distance_suivante_1 x = let (Just y) = distance x in let (Just z) = suivante y in distance z

-- l'opérateur <$> (fmap) est inutile pour cette tâche car on a pas de fonction à valeurs 
-- non monadiques à convertir (distance et suivante renvoient un "Maybe a")

bind_Maybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bind_Maybe Nothing _ = Nothing
bind_Maybe (Just x) f = f x

-- la différence entre bind_Maybe et fmap pour ce qui est de la capacité à renvoyer des erreurs est que:
-- bind_Maybe permet d'utiliser des fonctions qui peuvent renvoyer des erreurs (a -> Maybe b) sur des valeurs de type (Maybe a)
-- fmap permer d'utiliser des fonctions sans erreurs (a -> b) sur des valeurs de type (Maybe a)

distance_suivante_2 :: String -> Maybe Double
distance_suivante_2 x = Just x >>= distance >>= suivante >>= distance

distance_suivante_3 :: String -> Maybe Double
distance_suivante_3 x = do
    d <- distance x
    s <- suivante d
    distance s

-- EOF
