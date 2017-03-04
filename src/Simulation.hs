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
  ((), finalGS) <- runGame simulation initRS initGS
  return $! finalGS
  where
  simulation = do
    stepGame
    gs <- getGS
    unless (gs^.gsExit) simulation

stepGame :: Game ()
stepGame = do
  liftIO $ putStrLn "stepGame"
  evts <- translateVtyEvents
  liftIO $ putStrLn $ "evts = " ++ show evts
  processEvents evts
  updateDisplay

translateVtyEvents :: Game [Event]
translateVtyEvents = do
  liftIO $ putStrLn "translateVtyEvents"
  liftIO $ putStrLn "before getVty"
  vty <- getVty
  liftIO $ putStrLn "after getVty"
  k <- liftIO (Vty.nextEvent vty)
  liftIO $ putStrLn "nextEvent"
  case k == Vty.EvKey Vty.KEsc [] of
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
    _gs <- getGS
    -- TODO: move player
    liftIO $ putStrLn "Move Player"
    return ()
  Exit                -> exit
  Redraw              -> getVty >>= liftIO . Vty.refresh

