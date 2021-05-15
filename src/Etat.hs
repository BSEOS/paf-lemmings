module Etat where

import Coordonnees
import Environnement
import Lemming
import Niveau


data Etat = Etat
  { envE :: Env,
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
rassembleEnvniv (x1 : xs1) (x2 : xs2) = if x1 == ' ' then x2 : rassembleEnvniv xs1 xs2 else x1 : rassembleEnvniv xs1 xs2

showEtat :: Etat -> String
showEtat e = rassembleEnvniv (show (envE e)) (show (niveauE e))

instance Show Etat where
  show = showEtat

tourLemming :: Int -> Lemming -> Etat -> Etat
tourLemming n (Mort c) (Etat envi niv r v s) = Etat (enleveEnv n envi) niv r (v -1) s
tourLemming n (Marcheur Gauche c) (Etat envi niv r v s) =
  case trouveSortie niv of
    Nothing -> suite
    Just cs -> if cs == c then Etat (enleveEnv n envi) niv r (v -1) (s + 1) else suite
  where
    suite = case (passable (gauche c) niv && passable (haut (gauche c)) niv, dur (bas (gauche c)) niv) of
      (True, True) -> Etat (deplaceDansEnv n (gauche c) envi) niv r v s
      (_, False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Gauche 0 (gauche c)))) envi) niv r v s
      (_, _) -> Etat (appliqueIdEnv n (const (Lem n (Marcheur Droite c))) envi) niv r v s
tourLemming n (Marcheur Droite c) (Etat envi niv r v s) =
  case trouveSortie niv of
    Nothing -> suite
    Just cs -> if cs == c then Etat (enleveEnv n envi) niv r (v -1) (s + 1) else suite
  where
    suite = case (passable (droite c) niv && passable (haut (droite c)) niv, dur (bas (droite c)) niv) of
      (True, True) -> Etat (deplaceDansEnv n (droite c) envi) niv r v s
      (_, False) -> Etat (appliqueIdEnv n (const (Lem n (Tombeur Droite 0 (droite c)))) envi) niv r v s
      (_, _) -> Etat (appliqueIdEnv n (const (Lem n (Marcheur Gauche c))) envi) niv r v s
tourLemming n (Tombeur dir k c) (Etat envi niv r v s) =
  case (dur (bas c) niv, k >= hauteurMortelle) of
    (True, True) -> Etat (appliqueIdEnv n (const (Lem n (Mort c))) envi) niv r v s
    (True, _) -> Etat (appliqueIdEnv n (const (Lem n (Marcheur dir c))) envi) niv r v s
    (_, _) -> Etat (appliqueIdEnv n (const $ Lem n (Tombeur dir (k + 1) (bas c))) (deplaceDansEnv n (bas c) envi)) niv r v s

tourEntite :: Int -> Etat -> Etat
tourEntite n et = case trouveIdEnv n (envE et) of
  Nothing -> et
  Just (Lem _ l) -> tourLemming n l et

popLem :: Etat -> Etat
popLem (Etat envi niv r v s) = case trouveEntree niv of
  Nothing -> Etat envi niv r v s
  Just c -> Etat nenvi niv (r -1) (v + 1) s
    where
      nenvi = addEntite nlem envi
      nlem = Lem (idFrais envi) (Tombeur Droite 0 c)

tourEtat :: Int -> Etat -> Either Fin Etat
tourEtat t e = (verif . pop) $ foldr etape e (entitesEnv (envE e))
  where
    etape enti acc = tourEntite (idEnt enti) acc
    pop = if restants > 0 && (mod t 5) == 0 then popLem else id
    restants = lrestantsE e
    verif et =
      if lrestantsE et == 0 && lvivantsE et == 0
        then Left $ Victoire $ lsauvesE et
        else Right et