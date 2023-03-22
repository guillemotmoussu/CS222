{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Data.Array (Array)
import Data.List (intercalate)
{-# HLINT ignore "Use camelCase" #-}

data JSON =
    JSON_Int Integer |
    JSON_Bool Bool |
    JSON_String String |
    JSON_Array [JSON] |
    JSON_Object [(String, JSON)]

exemple_tableau :: JSON
exemple_tableau = JSON_Array [JSON_Int 42, JSON_Bool True, JSON_String "JSON!", JSON_Array [JSON_Bool False, JSON_Int 0]]

exemple_objet :: JSON
exemple_objet = JSON_Object [("entier", JSON_Int 42), ("texte", JSON_String "Haskell!"), ("tableau", JSON_Array [JSON_Bool True])]

-- Cela fait penser à la structure de données Bintree

show_json :: JSON -> String
show_json (JSON_Int i) = show i
show_json (JSON_Bool True) = "true"
show_json (JSON_Bool False) = "false"
show_json (JSON_String s) = show s
show_json (JSON_Array xs) = "[" ++ intercalate "," (map show_json xs) ++ "]"
show_json (JSON_Object kvs) = "{" ++ intercalate "," (map show_pair kvs) ++ "}"
  where
    show_pair (k, v) = show k ++ ":" ++ show_json v

instance Show JSON where
    show = show_json

data Task =
    PrintVal Integer |
    PrintSum Integer Integer

serialize_task :: Task -> JSON
serialize_task (PrintVal x) = JSON_Object [("op", JSON_String "PrintVal"), ("x", JSON_Int x)]
serialize_task (PrintSum x y) = JSON_Object [("op", JSON_String "PrintSum"), ("x", JSON_Int x), ("y", JSON_Int y)]

deserialize_task :: JSON -> Maybe Task
deserialize_task (JSON_Object [("op", JSON_String "PrintVal"), ("x", JSON_Int x)]) = Just (PrintVal x)
deserialize_task (JSON_Object [("op", JSON_String "PrintSum"), ("x", JSON_Int x), ("y", JSON_Int y)]) = Just (PrintSum x y)
deserialize_task _ = Nothing

class Serializable a where
    serialize :: a -> JSON
    deserialize :: JSON -> Maybe a

instance Serializable Task where
    serialize (PrintVal x) = JSON_Object [("op", JSON_String "PrintVal"), ("x", JSON_Int x)]
    serialize (PrintSum x y) = JSON_Object [("op", JSON_String "PrintSum"), ("x", JSON_Int x), ("y", JSON_Int y)]

    deserialize (JSON_Object [("op", JSON_String "PrintVal"), ("x", JSON_Int x)]) = Just (PrintVal x)
    deserialize (JSON_Object [("op", JSON_String "PrintSum"), ("x", JSON_Int x), ("y", JSON_Int y)]) = Just (PrintSum x y)
    deserialize _ = Nothing

instance Serializable a => Serializable [a] where
    serialize xs = JSON_Array (map serialize xs)

    deserialize (JSON_Array xs) = sequenceA (map deserialize xs)
    deserialize _ = Nothing
