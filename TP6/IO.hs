
import Data.List
import System.Environment

afficher_avec_etoiles :: Show a => a -> IO()
afficher_avec_etoiles x = putStrLn ("***"++show x++"***")

echo :: IO()
echo = getLine >>= putStrLn

wc :: String -> IO()
wc x = ((++ " " ++ x) <$> (\z -> show (length $ lines z) ++ " " ++ show (length $ words z) ++ " " ++ show (length z)) <$> readFile x) >>= putStrLn

main :: IO()
main = getArgs >>= (\x -> wc $ head x)

-- EOF
