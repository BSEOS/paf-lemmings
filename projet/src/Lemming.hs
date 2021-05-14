module Lemming where 

import Coordonnees

data Direction = Gauche | Droite
    deriving(Eq, Show)

data Lemming = Marcheur Direction Coordonnees 
                | Tombeur Direction Int Coordonnees
                | Mort Coordonnees
            deriving Eq

instance Show Lemming where 
    show (Mort _) = "+"
    show (Marcheur Gauche _) = "<"
    show (Marcheur Droite _) = ">"
    show (Tombeur _ _ _ ) = "V"

coordLemming :: Lemming -> Coordonnees
coordLemming (Marcheur _ c) = c   
coordLemming (Mort c) = c   
coordLemming (Tombeur _ _ c) = c

bougeLemming :: Deplacement  -> Lemming -> Lemming 
bougeLemming dep (Marcheur dir c) = Marcheur dir (bougeCoordonnees dep c)
bougeLemming dep (Mort c) = Mort (bougeCoordonnees dep c)
bougeLemming dep (Tombeur dir n c) = Tombeur dir n (bougeCoordonnees  dep c)

deplaceLemming :: Coordonnees -> Lemming -> Lemming
deplaceLemming c (Mort _) = Mort c 
deplaceLemming c (Marcheur d _) = Marcheur d c 
deplaceLemming c (Tombeur d n _) = Tombeur d n c

instance Placable Lemming where
    coordP = coordLemming
    bougeP = bougeLemming
    deplaceP = deplaceLemming

tuelemming :: Lemming -> Lemming 
tuelemming l = Mort (coordLemming l)

prop_tuelemming_post :: Lemming -> Bool 
prop_tuelemming_post l = case tuelemming l of
                            Mort c -> c == coordLemming l 
                            _ -> False

prop_bougeLemmingDHDBG :: Lemming -> Bool 
prop_bougeLemmingDHDBG l = bougeLemming D l == (bougeLemming G . bougeLemming DB . bougeLemming DH) l 