
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K

import Mouse (Coordonnee)
import qualified Mouse as M

import Data.Int (Int32)
import qualified Data.Int as I

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int }
  deriving (Show)

-- Verifie si c'est bien le personnage qui est clické
personnageClicked :: Coordonnee -> Coordonnee -> Bool 
personnageClicked (M.Coordonnee x y) (M.Coordonnee a b) = 
  if ((x>=a) && (x<= (a+100)))&&((y>=b)&&(y<=(b+100))) then True 
    else False

-- Permet d'extraire les coordonnees du personnage
gameStateToCoord :: GameState -> Coordonnee
gameStateToCoord (GameState x y s) = M.Coordonnee  (toInteger x) (toInteger y)

initGameState :: GameState
initGameState = GameState 200 300 4

moveLeft :: GameState -> GameState
moveLeft (GameState x y s) = GameState (x-s) y s

moveRight :: GameState -> GameState
moveRight (GameState x y s) = GameState (x+s) y s
                              
moveUp :: GameState -> GameState
moveUp (GameState x y s) = GameState x (y-s) s

moveDown :: GameState -> GameState
moveDown (GameState x y s) = GameState x (y+s) s

gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime =
  let modif = (if K.keypressed KeycodeLeft kbd
               then moveLeft else id)
              .
              (if K.keypressed KeycodeRight kbd
               then moveRight else id)
              .
              (if K.keypressed KeycodeUp kbd
               then moveUp else id)
              .
              (if K.keypressed KeycodeDown kbd
               then moveDown else id)

  in modif gstate
