{-# LANGUAGE OverloadedStrings #-}
module Random
( mkLevel
, generateLevelAndLoad
) where

import           Data.Array.IO
import           Control.Monad.State
import           Control.Lens hiding (Level)

import           Level
import           Game
import           Misc

mkLevel :: (Int,Int) -> Game Level
mkLevel (maxX,maxY) = do
  let l = mkEmptyLevel (0,0) (maxX,maxY)
  a <- liftIO (thaw (l^.levelTiles)) :: Game (IOArray Coord Tile)
  liftIO $ writeArray a (1,1) upStairs
  a' <- liftIO $ freeze a
  return $! l & levelTiles .~ a'

generateLevelAndLoad :: (Int, Int) -> Game ()
generateLevelAndLoad size = do
  lvl <- mkLevel size
  modifyGS (gsLevel .~ lvl)
  logMsg "After many winding passages you arrive at a new floor"

