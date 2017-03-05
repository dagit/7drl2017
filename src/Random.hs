module Random
( mkLevel
) where

import           Level
import           Game

mkLevel :: (Int,Int) -> Game Level
mkLevel (maxX,maxY) = do
  let l = mkEmptyLevel (0,0) (maxX,maxY)
  return $! l
  
