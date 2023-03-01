
data EtatMachine = Vide | Percolation | CafePret Int deriving (Show)

type InfoMachine = (Int, EtatMachine)

lancerMachine :: InfoMachine -> InfoMachine
lancerMachine (x,CafePret 4) = (x,CafePret 4)
lancerMachine (x,_) = (x,Percolation)

attendre :: InfoMachine -> InfoMachine
attendre (x,Vide) = (x,Vide)
attendre (x,CafePret y) = (x,CafePret y)
attendre (x,Percolation) = (x,CafePret 4)

servirCafe :: InfoMachine -> InfoMachine
servirCafe (x,Vide) = (x,Vide)
servirCafe (x,Percolation) = (x,Percolation)
servirCafe (i,CafePret 1) = (i+1,Vide)
servirCafe (i,CafePret x) = (i+1,CafePret (x-1))

executerActions :: InfoMachine -> [Char] -> InfoMachine
executerActions (x,y) [] = (x,y)
executerActions (x,y) ('L':as) = executerActions (lancerMachine (x,y)) as
executerActions (x,y) ('A':as) = executerActions (attendre (x,y)) as
executerActions (x,y) ('S':as) = executerActions (servirCafe (x,y)) as
executerActions (x,y) (_:as) = (0,Vide)
