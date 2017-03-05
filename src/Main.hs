{-# LANGUAGE ScopedTypeVariables #-}
module Main
( main )
where

import Control.Lens
import Control.Exception (SomeException, IOException, bracket, catch)

import Graphics.Vty

import Game
import Saves
import Simulation

withVty :: Config -> (Vty -> IO a) -> IO a
withVty cfg = bracket (mkVty cfg)
                      shutdown

main :: IO ()
main = gameMain `catch` (\(e::SomeException) -> print e)

gameMain:: IO ()
gameMain = do
  withVty defaultConfig $ \vty -> do
    mb_gs <- loadGame saveName
             -- Maybe we should let the user know?
             `catch` (\(_::IOException) -> return Nothing)
    let gs = maybe mkInitGameState
                   -- Make sure exit is not already requested, regardless
                   -- of how the state was saved.
                   (gsExit .~ False)
                   mb_gs
    let rs = mkRenderState vty
    final <- runSimulation rs gs
    saveGame saveName final
  putStrLn "Thanks for playing!"


