{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.Array (Array)
{-# HLINT ignore "Use camelCase" #-}

data JSON =
    JSON_Int Integer |
    JSON_Bool Bool |
    JSON_String String |
    JSON_Array [JSON] |
    JSON_Object [(String, JSON)]

true :: Bool
true = True

false :: Bool
false = False

exemple_tableau :: [JSON]
exemple_tableau = [JSON_Int 42, JSON_Bool true, JSON_String "JSON!", JSON_Array [false, 0]]

exemple_objet :: [(String, JSON)]
exemple_objet = [("entier",42), ("texte","Haskell!"), ("tableau",[true])]

-- Cela fait penser à la structure de données Bintree

show_json :: JSON -> String
show_json = 
