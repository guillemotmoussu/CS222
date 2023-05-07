{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module IO where
import Data.List
import System.Environment

afficher_avec_etoiles :: Show a => a -> IO ()
afficher_avec_etoiles x = putStrLn $ "***" ++ show x ++ "***"

echo :: IO ()
echo = do
  str <- getLine
  putStrLn str

wc :: String -> IO ()
wc filename = do
  str <- readFile filename
  let nb_lignes = length $ lines str
  let nb_mots = length $ words str
  let nb_octets = length str
  putStrLn $ show nb_lignes ++ " " ++ show nb_mots ++ " " ++ show nb_octets ++ " " ++ filename

main :: IO ()
main = do
  args <- getArgs
  mapM_ wc args