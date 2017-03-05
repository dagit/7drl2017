{-# LANGUAGE TemplateHaskell #-}
module Player
( Player(..)
, pCoord
, mkPlayer
, playerSymbol
)
where

import Control.Lens

import Graphics.Vty

import Misc

playerSymbol :: Symbol
playerSymbol = Symbol
  { _sChar = '@'
  , _sAttr = Just (defAttr `withForeColor` blue `withBackColor` green)
  }

data Player = Player
    { _pCoord :: !Coord
    } deriving (Read, Show, Eq, Ord)

mkPlayer :: Coord -> Player
mkPlayer c = Player
  { _pCoord = c
  }

makeLenses ''Player
