{-# LANGUAGE OverloadedStrings #-}
module Simulation
( runSimulation
) where

import           Data.Array
import           Control.Monad.State
import           Control.Lens hiding (Level)

import qualified Graphics.Vty as Vty

import           Action
import           Game
import           Event as E
import           Level as L
import           Player
import           Render
import           Random

-- | This is for playing a game through to a final game state
runSimulation :: RenderState -> GameState -> IO GameState
runSimulation initRS initGS = do
  ((), finalGS) <- runGame (bootstrap >> simulation)
                           initRS
                           initGS
  return $! finalGS
  where
  bootstrap = do
    -- Generate a new level
    gs  <- getGS
    lvl <- mkLevel (10,10)
    let gs' = gs & gsLevel .~ lvl
    putGS gs'
    logMsg "Move with the arrows keys. Press ESC to exit."
    updateDisplay
  simulation = do
    stepGame
    gs <- getGS
    unless (gs^.gsExit) simulation

stepGame :: Game ()
stepGame = do
  evts <- translateVtyEvents
  processEvents evts
  simulateWorld
  updateDisplay
  trimLog

trimLog :: Game ()
trimLog = modifyGS $ \gs -> gs & gsLog .~ take 50 (gs^.gsLog)

simulateWorld :: Game ()
simulateWorld = do
  gs <- getGS
  let lvl = gs^.gsLevel
  let playerTile = (lvl^.levelTiles) ! (gs^.gsPlayer^.pCoord)
  case playerTile^.tileInteraction of
    Passable     -> logMsg ""
    Trigger (Trap msg _dmg) -> logMsg msg
    Impassable   -> logMsg "Impossible!"
    -- i | isExit i -> logMsg "You found the exit!"
    _            -> logMsg ""
  return ()

translateVtyEvents :: Game [Event]
translateVtyEvents = do
  k <- getVty >>= liftIO . Vty.nextEvent
  case k == Vty.EvKey Vty.KEsc        [] ||
       k == Vty.EvKey (Vty.KChar 'q') [] of
    True -> return [E.Exit]
    _    -> return $
      case k of
        Vty.EvKey (Vty.KChar 'r') [Vty.MCtrl]  -> [Redraw]
        Vty.EvKey Vty.KLeft       []           -> [TryMovePlayerBy (-1) 0]
        Vty.EvKey Vty.KRight      []           -> [TryMovePlayerBy 1 0]
        Vty.EvKey Vty.KUp         []           -> [TryMovePlayerBy 0 (-1)]
        Vty.EvKey Vty.KDown       []           -> [TryMovePlayerBy 0 1]
        Vty.EvKey (Vty.KChar '>') []           -> [TryUpStairs]
        Vty.EvKey (Vty.KChar '<') []           -> [TryDownStairs]
        _                                      -> []

processEvents :: [Event] -> Game ()
processEvents = mapM_ processEvent

processEvent :: Event -> Game ()
processEvent e = case e of
  TryMovePlayerBy x y -> do
    gs <- getGS
    let lvl      = gs^.gsLevel
    let ((minX,minY),(maxX,maxY)) = bounds (lvl^.levelTiles)
    let playerX  = gs^.gsPlayer^.pCoord^._1
    let playerY  = gs^.gsPlayer^.pCoord^._2
    let playerX' = playerX + x
    let playerY' = playerY + y
    -- This checks the level bounds so that the player can't
    -- go out of level bounds.
    let playerX'' = if minX <= playerX' && playerX' < maxX
                      then playerX'
                      else playerX
    let playerY'' = if minY <= playerY' && playerY' < maxY
                      then playerY'
                      else playerY
    when (playerX == playerX'' || playerY == playerY'') $
      logMsg "Your movement is blocked"
    -- Now check that the desired tile can be moved on
    let t = (lvl^.levelTiles) ! (playerX'', playerY'')
    case t^.tileInteraction of
      Impassable -> return ()
      _          -> putGS (gs & (gsPlayer.pCoord._1) .~ playerX''
                              & (gsPlayer.pCoord._2) .~ playerY'')
    return ()
  TryUpStairs -> do
    gs <- getGS
    let lvl = gs^.gsLevel
    let playerXY = gs^.gsPlayer^.pCoord
    let pTile = (lvl^.levelTiles) ! playerXY
    case pTile^.tileInteraction of
      UpStairs -> do
        logMsg "You take the stairs up."
        generateLevelAndLoad (10,10)
      _        -> logMsg "There are no stairs up here."
  TryDownStairs -> do
    gs <- getGS
    let lvl = gs^.gsLevel
    let playerXY = gs^.gsPlayer^.pCoord
    let pTile = (lvl^.levelTiles) ! playerXY
    case pTile^.tileInteraction of
      DownStairs -> do
        logMsg "You take the stairs down."
        generateLevelAndLoad (10,10)
      _          -> logMsg "There are no stairs up here."
  E.Exit              -> exit
  Redraw              -> getVty >>= liftIO . Vty.refresh

