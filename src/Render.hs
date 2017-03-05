module Render
( updateDisplay
) where

import Data.Array

import Control.Monad.State
import Control.Lens

import Graphics.Vty

import Game
import Player
import Misc
import Level as L

updateDisplay :: Game ()
updateDisplay = do
    let info = string defAttr "Move with the arrows keys. Press ESC to exit."
    -- determine offsets to place the player in the center of the level.
    (w,h) <- (outputIface <$> getVty) >>= liftIO . displayBounds
    thePlayer <- _gsPlayer <$> getGS
    let ox = (w `div` 2) - (thePlayer ^. pCoord._1)
        oy = (w `div` 2) - (thePlayer ^. pCoord._2)
    -- translate the world images to place the player in the center of the
    -- level.
    world' <- map (translate ox oy) <$> worldImages
    let pic = picForLayers $ info : world'
    vty <- getVty
    liftIO $ update vty pic

worldImages :: Game [Image]
worldImages = do
  thePlayer <- _gsPlayer <$> getGS
  let playerImage = translate (thePlayer ^. pCoord._1)
                              (thePlayer ^. pCoord._2)
                              (symbolToImage playerSymbol)
  -- Update the cached level image, or use it if it's already
  -- defined.
  mb_lvlImage <- _rsImage <$> getRS
  lvlImage <- case mb_lvlImage of
    Just lvlImage -> return lvlImage
    Nothing       -> do
      (rs,gs) <- get
      let lvlImage = buildLevelImage (gs^.gsLevel)
      putRS (rs & rsImage .~ Just lvlImage)
      return lvlImage
  return [playerImage, lvlImage]

imageForTile :: Tile -> Image
imageForTile t = symbolToImage (t^.tileSymbol)

symbolToImage :: Symbol -> Image
symbolToImage s = char (maybe defAttr id (s^.sAttr)) (s^.sChar)

buildLevelImage :: L.Level -> Image
buildLevelImage l =
  let (w,h) = snd (bounds (l^.levelTiles))
  in vertCat [ row
             | y <- [0..h-1]
             , let row = horizCat [ i
                                  | x <- [0..w-1]
                                  , let i = imageForTile ((l^.levelTiles) ! (x,y))
                                  ]
             ]
