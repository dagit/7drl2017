module Main
( main )
where

import Control.Exception (bracket)

import Graphics.Vty

import Game
import Player
import Level
import Simulation

withVty :: Config -> (Vty -> IO a) -> IO a
withVty cfg = bracket (mkVty cfg)
                      shutdown

main :: IO ()
main = do
  final <- withVty defaultConfig $ \vty -> do
    let l  = mkLevel
    let gs = mkGameState (mkPlayer (5,5)) l
    let rs = mkRenderState vty
    runSimulation rs gs
  print final


