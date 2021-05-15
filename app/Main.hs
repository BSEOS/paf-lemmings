module Main where

import Environnement
import Etat
import Lib
import Moteur
import Niveau

etat1 :: Etat
etat1 =
  Etat
    { envE = Environnement.envide 5 5,
      niveauE = Niveau.chaineDeNiveau " ",
      lrestantsE = 10,
      lvivantsE = 0,
      lsauvesE = 0
    }

main :: IO ()
main = pure etat1 >>= lance >> return ()
