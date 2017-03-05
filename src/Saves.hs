module Saves
( saveName
, saveGame
, loadGame
) where

import Text.Read

import Game

saveName :: FilePath
saveName = "7drl2017.sav"

saveGame :: FilePath -> GameState -> IO ()
saveGame fp gs = writeFile fp (show gs)

loadGame :: FilePath -> IO (Maybe GameState)
loadGame fp = readMaybe <$> readFile fp
