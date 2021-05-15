module Main where

import Environnement
import Etat
import Lib
import Moteur
import qualified Niveau

exempleNiveau1 :: Niveau.Niveau
exempleNiveau1 = read "XXXXXXXXX\nX E     X\nX       X\nX0000   X\nX       X\nX       X\nX   0000X\nX       X\nX       X\nX 000000X\nX       X\nX     S X\nXXXXXXXXX"

etat1 :: Etat
etat1 =
  Etat
    { envE = Environnement.envide 13 9,
      niveauE = exempleNiveau1,
      lrestantsE = 10,
      lvivantsE = 0,
      lsauvesE = 0
    }

main :: IO ()
main = pure etat1 >>= lance >> return ()
