module Etat where

import Coordonnees
import Lemming
import Environnement
import Niveau

data Etat = Etat {
    envE :: Env,
    niveauE :: Niveau,
    lrestantsE :: Int,
    lvivantsE :: Int, 
    lsauvesE :: Int
}

data Fin = Victoire Int | Defaite

hauteurMortelle :: Int
hauteurMortelle = 5

rassembleEnvniv :: String -> String -> String 
rassembleEnvniv [] _ = []
rassembleEnvniv _ [] = []
rassembleEnvniv (x1:xs1) (x2:xs2) = if x1 == ' ' then x2:rassembleEnvniv xs1 xs2 else x1:rassembleEnvniv xs1 xs2 

showEtat :: Etat -> String 
showEtat e = rassembleEnvniv (show (envE e)) (show (niveauE e))

instance Show Etat where 
    show = showEtat

tourLemming :: Int -> Lemming -> Etat -> Etat 
tourLemming n (Mort c) (Etat envi niv r v s) = Etat (enleveEnv n envi) niv r (v-1) s 
--tourLemming n (Marcheur Gauche c) (Etat envi niv r m s) = case trouveSortie niv of 
      --                                                      Nothing -> suite
    --                                                        Just cs -> if cs == c then Etat (enleveEnv n envi) niv e (m-1) (s+1) else suite
  --                                                          where suite = case (pasable (gauche c) niv && passable gaut (gauche c))

tourEntite :: Int -> Etat -> Etat
tourEntite n et = case trouveIdEnv n (envE et) of 
                    Nothing -> et
                    Just (Lem _ l) -> tourLemming n l et

popLem :: Etat -> Etat
popLem (Etat envi niv r v s) = case trouveEntree niv of 
                                Nothing -> Etat envi niv r v s
                                Just c -> Etat nenvi niv (r-1) (v+1) s 
                                    where nenvi = addEntite nlem envi 
                                          nlem = Lem (idFrais envi) (Tombeur Droite 0 c)

tourEtat :: Int -> Etat -> Either Fin Etat
tourEtat t e = (verif . pop) $ foldr etape e (entitesEnv (envE e))
    where etape enti acc = tourEntite (idEnt enti) acc
          pop = if restants > 0 && (mod t 5) == 0 then popLem else id
          restants = lrestantsE e 
          verif et = if lrestantsE et == 0 && lvivantsE et == 0
                        then Left $ Victoire $ lsauvesE et
                        else Right et