module Main
( main )
where

import Control.Exception (bracket)

import Graphics.Vty

import Game
import Player
import Level
import Simulation

withVty :: Config -> (Vty -> IO a) -> IO ()
withVty cfg m = bracket (mkVty cfg) m shutdown

main :: IO ()
main = withVty defaultConfig $ \vty -> do
  let l  = mkLevel
  let gs = mkGameState (mkPlayer (5,5)) l
  let rs = mkRenderState vty
  final <- runSimulation rs gs
  print final


