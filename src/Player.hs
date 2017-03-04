{-# LANGUAGE TemplateHaskell #-}
module Player
( Player(..)
, pCoord
, mkPlayer
)
where

import Control.Lens

import Misc

data Player = Player
    { _pCoord :: !Coord
    } deriving (Read, Show, Eq, Ord)

mkPlayer :: Coord -> Player
mkPlayer c = Player
  { _pCoord = c
  }

makeLenses ''Player
