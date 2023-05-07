{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

import Data.List
import System.Environment

afficher_avec_etoiles :: Show a => a -> IO()
afficher_avec_etoiles x = putStrLn ("***"++show x++"***")

echo :: IO()
echo = getLine >>= putStrLn

wc :: String -> IO()
wc x = readFile x >>= putStrLn . ((++ " " ++ x) <$> (\z -> show (length $ lines z) ++ " " ++ show (length $ words z) ++ " " ++ show (length z)))

main :: IO()
main = getArgs >>= (wc . head)

-- EOF
