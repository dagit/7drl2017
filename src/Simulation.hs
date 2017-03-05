module Simulation
( runSimulation
) where

import           Control.Monad.State

import           Control.Lens

import qualified Graphics.Vty as Vty

import           Game
import           Event
import           Player
import           Render

-- | This is for playing a game through to a final game state
runSimulation :: RenderState -> GameState -> IO GameState
runSimulation initRS initGS = do
  ((), finalGS) <- runGame (updateDisplay >> simulation)
                           initRS
                           initGS
  return $! finalGS
  where
  simulation = do
    stepGame
    gs <- getGS
    unless (gs^.gsExit) simulation

stepGame :: Game ()
stepGame = do
  evts <- translateVtyEvents
  processEvents evts
  updateDisplay

translateVtyEvents :: Game [Event]
translateVtyEvents = do
  k <- getVty >>= liftIO . Vty.nextEvent
  case k == Vty.EvKey Vty.KEsc        [] ||
       k == Vty.EvKey (Vty.KChar 'q') [] of
    True -> return [Exit]
    _    -> return $
      case k of
        Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl]  -> [Redraw]
        Vty.EvKey Vty.KLeft       []           -> [TryMovePlayerBy (-1) 0]
        Vty.EvKey Vty.KRight      []           -> [TryMovePlayerBy 1 0]
        Vty.EvKey Vty.KUp         []           -> [TryMovePlayerBy 0 (-1)]
        Vty.EvKey Vty.KDown       []           -> [TryMovePlayerBy 0 1]
        _                                      -> []

processEvents :: [Event] -> Game ()
processEvents = mapM_ processEvent

processEvent :: Event -> Game ()
processEvent e = case e of
  TryMovePlayerBy x y -> do
    gs <- getGS
    let lvl      = gs^.gsLevel
    let playerX  = gs^.gsPlayer^.pCoord^._1
    let playerY  = gs^.gsPlayer^.pCoord^._2
    let playerX' = playerX + x
    let playerY' = playerY + y
    -- TODO: check bounds
    putGS (gs & (gsPlayer.pCoord._1) .~ playerX'
              & (gsPlayer.pCoord._2) .~ playerY')
    return ()
  Exit                -> exit
  Redraw              -> getVty >>= liftIO . Vty.refresh

