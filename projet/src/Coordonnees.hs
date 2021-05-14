module Coordonnees where

data Coordonnees = C Int Int
    deriving (Show, Eq)

creeCoord :: Int -> Int -> Coordonnees
creeCoord = C

gauche :: Coordonnees -> Coordonnees
gauche (C a b) = C (a - 1 ) b

droite :: Coordonnees -> Coordonnees
droite (C a b) = C (a + 1) b

bas :: Coordonnees -> Coordonnees
bas (C a b) = C a (b - 1)

haut :: Coordonnees -> Coordonnees
haut (C a b) = C a (b + 1)

instance Ord Coordonnees where
        (<=) (C a1 b1) (C a2 b2) = (b1 > b2) || (b1 == b2 && a1 <= a2)

data Deplacement = N | G | D | H | B | GH | GB | DH | DB deriving(Eq, Show)        

bougeCoordonnees :: Deplacement -> Coordonnees -> Coordonnees
bougeCoordonnees N c = c
bougeCoordonnees G (C a b) = C (a-1) b
bougeCoordonnees GH (C a b) = C (a-1) (b+1)
bougeCoordonnees GB (C a b) = C (a-1) (b-1)
bougeCoordonnees D (C a b) = C (a+1) b
bougeCoordonnees DH (C a b) = C (a+1) (b+1)
bougeCoordonnees DB (C a b) = C (a+1) (b-1)
bougeCoordonnees H (C a b) = C a (b+1)
bougeCoordonnees B (C a b) = C a (b-1)

prop_bougeCoordonneesGaucheDroite :: Coordonnees -> Bool
prop_bougeCoordonneesGaucheDroite c = c == (bougeCoordonnees G . bougeCoordonnees D) c

prop_bougeCoordonneesGaucheHaut :: Coordonnees -> Bool 
prop_bougeCoordonneesGaucheHaut c = (bougeCoordonnees G . bougeCoordonnees H) c == bougeCoordonnees GH c

class Placable a where 
    coordP :: a -> Coordonnees
    bougeP :: Deplacement -> a -> a
    deplaceP :: Coordonnees -> a -> a

prop_placableGauche_law :: (Placable a, Eq a) => a -> Bool
prop_placableGauche_law x = bougeP G x == deplaceP (gauche (coordP x)) x