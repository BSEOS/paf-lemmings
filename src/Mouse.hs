module Mouse where

import SDL

import SDL.Vect (Point, V2)
import qualified SDL.Vect as V

import Data.List (foldl')

import Data.Set (Set)
import qualified Data.Set as S

import Data.Int (Int32)
import qualified Data.Int as In

data Coordonnee = Coordonnee Integer Integer
    deriving(Show, Eq)

createMouse :: Coordonnee
createMouse = Coordonnee 0 0
-- >>> deconvertD (P(V2 214 308))
-- V2 214 308
deconvertD :: Point V2 Int32 -> V2 Int32
deconvertD (P(V2 a b)) = V2 a b

inMaybe :: Maybe a -> a
inMaybe (Just x) = x

-- >>> deconvert (V2 214 308)
-- Coordonnee 214 308
deconvert :: V2 Int32 -> Coordonnee
deconvert (V2 a b) = Coordonnee (toInteger a) (toInteger b)


-- >>> coordonneeClick (P(V2 214 308))
-- Coordonnee 214 308
--Permet de recuperer les coordonnees de la souri au click
coordonneeClick ::  Point V2 Int32 -> Coordonnee
coordonneeClick p = deconvert $ deconvertD p

mouseClicked :: Event -> Maybe (Point V2 Int32)
mouseClicked event = 
    case eventPayload event of 
        MouseButtonEvent k@(MouseButtonEventData _ _ _ _ _ p)-> Just p
        _ -> Nothing

-- | prise en compte des événements SDL2 pour mettre à jour 
handleEvents :: [Event] ->  Maybe Coordonnee 
handleEvents [] = Nothing
handleEvents (x:ev) = if mouseClicked x /= Nothing  then  Just $ coordonneeClick $ inMaybe $ mouseClicked x
                            else handleEvents ev