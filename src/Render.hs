module Render
( updateDisplay
) where

import Control.Monad.State
import Control.Lens

import Graphics.Vty

import Game
import Player
import Level

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
                              (char (defAttr `withForeColor` blue `withBackColor` green)
                                    '@')
  return [playerImage]
-- worldImages :: Game [Image]
-- worldImages = do
--     thePlayer <- gets player
--     theLevel <- gets level
--     let playerImage = translate (playerX thePlayer) (playerY thePlayer) (char pieceA '@')
--     return [playerImage, levelGeoImage theLevel]
