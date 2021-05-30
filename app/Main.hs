{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module Main where
import Control.Monad ( unless, foldM )
import Control.Concurrent (threadDelay)
import Data.Map
import qualified Data.Map as Map
import qualified Data.Sequence as S
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
import Lemming
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

mapNiveau :: String -> Niveau.Niveau
mapNiveau = read

etat1 :: String -> Etat
etat1 s =
  Etat
    { envE = Environnement.envide 13 9,
      niveauE = mapNiveau s,
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
  s <- readFile "assets/Map1.txt"
  initializeAll
  window <- createWindow "Minijeu" $ defaultWindow { windowInitialSize = V2 640  520}
  renderer <- createRenderer window (-1) defaultRenderer
  -- chargement de l'image du fond
  (tmap, smap) <- loadBackground renderer "assets/background.bmp" TM.createTextureMap SM.createSpriteMap
  -- chargement des images
  (tmap, smap) <- loadImage renderer "assets/terre.bmp" tmap smap "terre"
  (tmap, smap) <- loadImage renderer "assets/metal.bmp" tmap smap "metal"
  (tmap, smap) <- loadImage renderer "assets/porte.bmp" tmap smap "porte"
  (tmap, smap) <- loadImage renderer "assets/lemming_droite.bmp" tmap smap "lemming_droite"
  (tmap, smap) <- loadImage renderer "assets/lemming_gauche.bmp" tmap smap "lemming_gauche"
  -- initialisation de l'état du jeu
  let gameState = M.initGameState
  -- initialisation de l'état du clavier
  let kbd = K.createKeyboard
  -- initialisation de l'état de la souri
  let mous = Mo.createMouse
  -- lancement de la gameLoop
  gameLoop 60 renderer tmap smap kbd gameState mous (etat1 s) s

gameLoop :: (RealFrac a, Show a) => a -> Renderer -> TextureMap -> SpriteMap -> Keyboard -> GameState -> Coordonnee -> Etat -> String -> IO ()
gameLoop frameRate renderer tmap smap kbd gameState mous etat s = do
  startTime <- time
  case tourEtat 0 etat of
    Right ne -> do
      events <- pollEvents
      let kbd' = K.handleEvents events kbd
      let mo = Mo.handleEvents events
      if mo /= Nothing && M.personnageClicked (inMaybe mo) (M.gameStateToCoord  gameState) then putStrLn $ "# Touché ! #" <> show (inMaybe mo) else putStr ""
      clear renderer
      threadDelay 100000
  --- display background
      S.displaySprite renderer tmap (SM.fetchSprite (SpriteId "background") smap)
  --display la terre
      let (Niveau.Niveau h l map) = mapNiveau s
      let listTerres = Map.keys $ filterWithKey (\k x -> Just x == Just Niveau.Terre) map
      displayImage renderer tmap smap "terre" listTerres
  --display metal
      let listmetaux = Map.keys $ filterWithKey (\k x -> Just x == Just Niveau.Metal ) map
      displayImage renderer tmap smap "metal" listmetaux
  --display porte d'entree
      let entree = Map.keys $ filterWithKey (\k x -> Just x == Just Niveau.Entree  ) map
      displayImage renderer tmap smap "porte" entree
    --display porte de sortie
      let sortie = Map.keys $ filterWithKey (\k x -> Just x == Just Niveau.Sortie) map
      displayImage renderer tmap smap "porte" sortie
  --------------------------------------
      let (Env h l entites cases) = envE ne
      let lemmingsMap = filterWithKey (\k x -> not $ S.null $ S.filter verifyIfIsLemming x) cases
      afficheLemmings renderer tmap smap lemmingsMap 0 0
  ------------------------------------
      present renderer
      endTime <- time
      let refreshTime = endTime - startTime
      let delayTime = floor ((1.0 / frameRate - refreshTime) * 1000)
      threadDelay $ delayTime * 1000 -- microseconds
      endTime <- time
      let deltaTime = endTime - startTime
      let gameState' = M.gameStep gameState kbd' deltaTime
      unless (K.keypressed KeycodeEscape kbd') (gameLoop frameRate renderer tmap smap kbd' gameState' mous ne s)

displayImage :: Renderer -> TextureMap -> SpriteMap -> String -> [Coordonnees]  -> IO ()
displayImage renderer tmap smap id [] = return ()
displayImage renderer tmap smap id ((C x y):as) = do
  S.displaySprite renderer tmap (S.moveTo (SM.fetchSprite (SpriteId id) smap) (fromIntegral x * 40) (fromIntegral y *40) )
  displayImage renderer tmap smap id as

verifyIfIsLemming :: Entite -> Bool 
verifyIfIsLemming (Lem _ _) = True

afficheLemmings :: Renderer -> TextureMap -> SpriteMap -> Map Coordonnees  (S.Seq Entite) -> CInt -> CInt -> IO ()
afficheLemmings renderer tmap smap lemmings transx transy = do
  let marcheursDroite = Map.keys $ filterWithKey (\k v -> not $ S.null $ S.filter estLemmigDroite v) lemmings
  let marcheursGauche = Map.keys $ filterWithKey (\k v -> not $ S.null $ S.filter estLemmigGauche v) lemmings
  displayImage renderer tmap smap "lemming_droite" marcheursDroite
  displayImage renderer tmap smap "lemming_gauche" marcheursGauche

estLemmigDroite :: Entite -> Bool 
estLemmigDroite (Lem _ (Tombeur droite _ _)) = True
estLemmigDroite (Lem _ (Marcheur droite _)) = True
estLemmigDroite (Lem _ _) = False

estLemmigGauche :: Entite -> Bool 
estLemmigGauche (Lem _ (Tombeur gauche _ _)) = True
estLemmigGauche (Lem _ (Marcheur gauche _)) = True
estLemmigGauche (Lem _ _) = False