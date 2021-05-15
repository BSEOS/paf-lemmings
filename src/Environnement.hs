module Environnement where

import Coordonnees
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Maybe as Y
import qualified Data.Sequence as S
import Lemming
import Niveau

data Entite = Lem Int Lemming
  deriving (Eq)

data Env = Env
  { hEnv :: Int,
    lEnv :: Int,
    entitesEnv :: S.Seq Entite,
    casesEnv :: M.Map Coordonnees (S.Seq Entite)
  } 

instance Show Entite where
  show (Lem _ l) = show l

instance Placable Entite where
  coordP (Lem _ l) = coordP l
  bougeP d (Lem i l) = Lem i $ bougeP d l
  deplaceP c (Lem i l) = Lem i $ deplaceP c l

idEnt :: Entite -> Int
idEnt (Lem i _) = i

envide :: Int -> Int -> Env
envide h l = Env h l S.empty M.empty

entitesEnv2 :: Env -> S.Seq Entite
entitesEnv2 (Env h l _ cases) = M.foldl' etape S.Empty cases
  where
    etape acc s = s <> acc

trouveIdSeq :: Int -> S.Seq Entite -> Maybe Entite
trouveIdSeq n = foldr etape Nothing
  where
    etape e acc = if idEnt e == n then Just e else acc

trouveIdEnv :: Int -> Env -> Maybe Entite
trouveIdEnv n = trouveIdSeq n . entitesEnv

trouveIdMap :: Int -> M.Map Coordonnees (S.Seq Entite) -> Maybe Coordonnees
trouveIdMap n = M.foldrWithKey etape Nothing
  where
    etape c s acc = case trouveIdSeq n s of
      Nothing -> acc
      Just _ -> Just c

prop_envInclusion1 :: Env -> Bool
prop_envInclusion1 (Env _ _ ents cases) = foldr etape True ents
  where
    etape e acc = case trouveIdMap (idEnt e) cases of
      Nothing -> False
      Just c -> c == coordP e

prop_envinclusion2 :: Env -> Bool
prop_envinclusion2 (Env _ _ ents cases) = M.foldrWithKey etape True cases
  where
    etape c s acc = foldr (etape2 c) acc s
    etape2 c e acc = case trouveIdSeq (idEnt e) ents of
      Nothing -> False
      Just e2 -> acc && coordP e2 == c && coordP e2 == coordP e

prop_env_inv :: Env -> Bool
prop_env_inv env = prop_envInclusion1 env && prop_envinclusion2 env

instance Show Env where
  show = showEnv

showEnv :: Env -> String
showEnv (Env h l _ cases) = let s = aux 0 (h -1) in s
  where
    aux x y =
      if x == (l -1)
        then (if y == 0 then lacase x y else lacase x y ++ "\n" ++ aux 0 (y -1))
        else lacase x y ++ aux (x + 1) y
    lacase x y = case M.lookup (C x y) cases of
      Nothing -> " "
      Just S.Empty -> " "
      Just (e S.:<| es) -> show e

caseVide :: Coordonnees -> Env -> Bool
caseVide (C x y) (Env h l _ cases) = (x < l) && (x >= 0) && (y < h) && (y >= 0)

appliqueIdSeq :: Int -> (Entite -> Entite) -> S.Seq Entite -> S.Seq Entite
appliqueIdSeq i f = foldr etape S.empty
  where
    etape n acc
      | idEnt n == i = f n S.:<| acc
      | otherwise = n S.:<| acc

appliqueIdEnv :: Int -> (Entite -> Entite) -> Env -> Env
appliqueIdEnv n f (Env h l ents cases) = case trouveIdSeq n ents of
  Nothing -> error $ "appliqueidEnv : n'a pas trouvé l'entité" ++ show n ++ " dans la sequences "
  Just e -> Env h l nents ncases
    where
      nents = appliqueIdSeq n f ents
      ncases = case trouveIdMap n cases of
        Nothing -> error $ "appliqueIdEnv : n'a pas trouvé l'entité " ++ show n
        Just endroit -> M.foldrWithKey etape M.empty cases
          where
            etape co s
              | co == endroit = M.insert co (appliqueIdSeq n f s)
              | otherwise = M.insert co s

enleveId :: Int -> S.Seq Entite -> S.Seq Entite
enleveId i = foldr etape S.empty
  where
    etape e acc
      | idEnt e == i = acc
      | otherwise = e S.:<| acc

enleveEnv :: Int -> Env -> Env
enleveEnv n (Env h l ents cases) = Env h l nents ncases
  where
    nents = enleveId n ents
    ncases = case trouveIdMap n cases of
      Nothing -> cases
      Just endroit -> case M.lookup endroit cases of
        Nothing -> undefined
        Just s -> M.insert endroit (enleveId n s) cases

deplaceDansEnv :: Int -> Coordonnees -> Env -> Env
deplaceDansEnv n dest (Env h l ents cases) = case trouveIdSeq n ents of
  Nothing -> error $ "deplaceDansEnv : pas trouve " ++ show n
  Just e -> Env h l nents ncases
    where
      nents = appliqueIdSeq n (deplaceP dest) ents
      ncases = case trouveIdMap n cases of
        Nothing -> error $ "deplce Dans env : entité : " ++ show n
        Just source ->
          let dents = Y.fromMaybe S.empty $ M.lookup dest cases
           in let sents = Y.fromMaybe S.empty $ M.lookup source cases
               in let ncases = M.insert source (enleveId n sents) cases
                   in M.insert dest (deplaceP dest e S.:<| dents) ncases

idFrais :: Env -> Int
idFrais (Env h l ents cases) = 1 + foldr etape 0 ents
  where
    etape ent = max (idEnt ent)

addEntite :: Entite -> Env -> Env
addEntite ent (Env h l ents cases) = Env h l nents ncases
  where
    nents = ent S.:<| ents
    ncases = M.insert (coordP ent) (ent S.:<| cents) cases
      where
        cents = Y.fromMaybe S.empty $ M.lookup (coordP ent) cases