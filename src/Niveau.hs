module Niveau where

import Coordonnees
import qualified Data.List as L
import qualified Data.Map.Strict as M

data Case = Metal | Terre | Entree | Sortie | Vide
  deriving (Eq)

instance Show Case where
  show Vide = " "
  show Metal = "X"
  show Terre = "0"
  show Entree = "E"
  show Sortie = "S"

chaineDeCase :: String -> Case
chaineDeCase " " = Vide
chaineDeCase "X" = Metal
chaineDeCase "0" = Terre
chaineDeCase "E" = Entree
chaineDeCase "S" = Sortie
chaineDeCase _ = Vide

instance Read Case where
  readsPrec _ x = [(chaineDeCase x, "")]

data Niveau = Niveau
  { hNivau :: Int,
    lNiveau :: Int,
    casesNiveau :: M.Map Coordonnees Case
  }
  deriving (Eq)

instance Show Niveau where
  show (Niveau h l cases) = let (s, _, _) = M.foldl' aux ("", 0, 0) cases in s
    where
      aux (s, x, y) v =
        if x == (l - 1)
          then (if y == (h -1) then (s ++ show v, 0, 0) else (s ++ show v ++ "\n", 0, y + 1))
          else (s ++ show v, x + 1, y)

chaineDeNiveau :: String -> Niveau
chaineDeNiveau = (\(cases, l, h) -> Niveau (h + 1) l cases) . L.foldl' aux (M.empty, 0, 0)
  where
    aux (cases, x, y) '\n' = (cases, 0, y + 1)
    aux (cases, x, y) c = (M.insert (C x y) (read [c]) cases, x + 1, y)

retourneNiveau :: Niveau -> Niveau
retourneNiveau (Niveau h l cases) = Niveau h l $ M.foldrWithKey etape M.empty cases
  where
    etape (C x y) c = M.insert (C x (h -1 - y)) c

instance Read Niveau where
  readsPrec _ x = [(retourneNiveau (chaineDeNiveau x), "")]

prop_niveauFerme :: Niveau -> Bool
prop_niveauFerme (Niveau h l cases) = M.foldrWithKey etape True cases
  where
    etape (C x y) c acc
      | x == 0 || x == l - 1 || y == 0 || y == h - 1 = (c == Metal) && acc
      | otherwise = acc

--exempleNiveau1
--exempleNiveau2 ....

prop_niveauEntreeSortie :: Niveau -> Bool
prop_niveauEntreeSortie (Niveau h l cases) = let (e, s) = M.foldrWithKey etape (0, 0) cases in e == 1 && s == 1
  where
    etape (C x y) Entree (en, so) = (en + 1, so)
    etape (C x y) Sortie (en, so) = (en, so + 1)
    etape (C x y) _ acc = acc

trouveEntree :: Niveau -> Maybe Coordonnees
trouveEntree (Niveau h l cases) = M.foldrWithKey etape Nothing cases
  where
    etape c Entree _ = Just c
    etape _ _ acc = acc

-- trouve le coordonnees de la case sortie
trouveSortie :: Niveau -> Maybe Coordonnees
trouveSortie (Niveau h l cases) = M.foldrWithKey etape Nothing cases
  where
    etape c Sortie _ = Just c
    etape _ _ acc = acc

prop_niveauENtreeCorrecte :: Niveau -> Bool
prop_niveauENtreeCorrecte niv = case trouveEntree niv of
  Nothing -> False
  Just (C x y) -> case M.lookup (C x (y - 1)) (casesNiveau niv) of
    Just Vide -> True
    _ -> False

prop_niveauSortieCorrecte :: Niveau -> Bool
prop_niveauSortieCorrecte niv = case trouveSortie niv of
  Nothing -> False
  Just (C x y) -> case M.lookup (C x (y -1)) (casesNiveau niv) of
    Just Metal -> True
    _ -> False

prop_niveauInclusion1 :: Niveau -> Bool
prop_niveauInclusion1 (Niveau h l cases) = M.foldrWithKey etape True cases
  where
    etape (C x y) _ acc = acc && (x >= 0) && (x < 1) && (y >= 0) && (y < h)

prop_niveauInclusion2 :: Niveau -> Bool
prop_niveauInclusion2 (Niveau h l cases) = aux (h -1) (l -1)
  where
    aux x y = case M.lookup (C x y) cases of
      Nothing -> False
      Just _ -> if x == 0 then y == 0 || aux (h -1) (y -1) else aux (x -1) (y -1)

-- plus un test
prop_niveauInvariant :: Niveau -> Bool
prop_niveauInvariant niv = prop_niveauFerme niv && prop_niveauEntreeSortie niv && prop_niveauENtreeCorrecte niv && prop_niveauSortieCorrecte niv

passable :: Coordonnees -> Niveau -> Bool
passable c (Niveau h l cases) = case M.lookup c cases of
  Just Vide -> True
  Just Entree -> True
  Just Sortie -> True
  _ -> False

dur :: Coordonnees -> Niveau -> Bool
dur c (Niveau h l cases) = case M.lookup c cases of
  Just Metal -> True
  Just Terre -> True
  _ -> False
