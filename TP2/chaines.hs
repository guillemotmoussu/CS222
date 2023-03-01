
repeterNFois :: Int -> Char -> [Char]
repeterNFois 0 _ = []
repeterNFois n c = c:repeterNFois (n-1) c

repeterNFoisBis :: Int -> a -> [a]
repeterNFoisBis 0 _ = []
repeterNFoisBis n c = c:repeterNFoisBis (n-1) c
-- ça n'était donc pas spécique au type Char

etoiles :: Int -> [Char] -> [Char]
etoiles n x = repeterNFois n '*' ++ x ++ repeterNFois n '*'

slashes :: [Char] -> [Char]
slashes x = "/" ++ x ++ "/"

commentaireDocumentation :: [Char] -> [Char]
commentaireDocumentation = (.) slashes (etoiles 2)
