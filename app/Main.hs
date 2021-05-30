{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad ( unless, foldM )
import Control.Concurrent (threadDelay)

import Data.Map
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (foldl')

import Foreign.C.Types (CInt (..) )

import Data.Int (Int32)
import qualified Data.Int as In

import SDL
import SDL.Time (time, delay)
import Linear (V4(..))

import TextureMap (TextureMap, TextureId (..))
import qualified TextureMap as TM

import Sprite (Sprite)
import qualified Sprite as S

import SpriteMap (SpriteMap, SpriteId (..))
import qualified SpriteMap as SM

import Keyboard (Keyboard)
import qualified Keyboard as K

import Coordonnees

import Mouse (Coordonnee, inMaybe)
import qualified Mouse as Mo

import qualified Debug.Trace as T

import Model (GameState)
import qualified Model as M

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

loadBackground :: Renderer-> FilePath -> TextureMap -> SpriteMap -> IO (TextureMap, SpriteMap)
loadBackground rdr path tmap smap = do
  tmap' <- TM.loadTexture rdr path (TextureId "background") tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId "background") (S.mkArea 0 0 640 480)
  let smap' = SM.addSprite (SpriteId "background") sprite smap
  return (tmap', smap')

loadImage :: Renderer-> FilePath -> TextureMap -> SpriteMap -> String -> IO (TextureMap, SpriteMap)
loadImage rdr path tmap smap identifiant = do
  tmap' <- TM.loadTexture rdr path (TextureId identifiant) tmap
  let sprite = S.defaultScale $ S.addImage S.createEmptySprite $ S.createImage (TextureId identifiant) (S.mkArea 0 0 45 45)
  let smap' = SM.addSprite (SpriteId identifiant) sprite smap
  return (tmap', smap')

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 640  520}
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/background.bmp" TM.createTextureMap SM.createSpriteMap
  -- chargement des images
  (tmap, smap) <- loadImage renderer "assets/terre.bmp" tmap smap "terre"
  (tmap, smap) <- loadImage renderer "assets/metal.bmp" tmap smap "metal"
  -- initialisation de l'état du jeu
  let gameState = M.initGameState
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- initialisation de l'état de la souri
  let mous = Mo.createMouse
  -- lancement de la gameLoop
  gameLoop 60 renderer tmap smap kbd gameState mous

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> GameState -> Coordonnee ->IO ()
gameLoop frameRate renderer tmap smap kbd gameState mous= do
  startTime <- time
  events <- pollEvents
  let kbd' = K.handleEvents events kbd
  let mo = Mo.handleEvents events
  if mo /= Nothing && M.personnageClicked (inMaybe mo) (M.gameStateToCoord  gameState) then putStrLn $ "# Touché ! #" <> show (inMaybe mo) else putStr ""
  clear renderer
  --- display background
  S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  --display la terre
  let (Niveau.Niveau h l map) = exempleNiveau1
  let listTerres = Map.keys $ filterWithKey (\k x -> Just x == Just Niveau.Terre) map
  displayImage renderer tmap smap "terre" listTerres
  --------------------------------------
  --display metal
  let listmetaux = Map.keys $ filterWithKey (\k x -> Just x == Just Niveau.Metal ) map
  displayImage renderer tmap smap "metal" listmetaux
  --------------------------------------
  ---
  present renderer
  endTime <- time
  let refreshTime = endTime - startTime
  let delayTime = floor ((1.0 / frameRate - refreshTime) * 1000)
  threadDelay $ delayTime * 1000 -- microseconds
  endTime <- time
  let deltaTime = endTime - startTime
  -- putStrLn $ "Delta time: " <> (show (deltaTime * 1000)) <> " (ms)"
  -- putStrLn $ "Frame rate: " <> (show (1 / deltaTime)) <> " (frame/s)"
  --- update du game state
  let gameState' = M.gameStep gameState kbd' deltaTime
  unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameState' mous)

displayImage :: Renderer -> TextureMap -> SpriteMap -> String -> [Coordonnees]  -> IO ()
displayImage renderer tmap smap id [] = return ()
displayImage renderer tmap smap id ((C x y):as) = do
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId id) smap) (fromIntegral x * 40) (fromIntegral y *40) )
  displayImage renderer tmap smap id as