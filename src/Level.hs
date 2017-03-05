{-# LANGUAGE TemplateHaskell #-}
module Level
( Level(..)
, Tile(..)
, Interaction(..)
, mkLevel
, tileSymbol
, tileInteraction
, tileInventory
, levelMobs
, levelTiles
) where

import           Data.Array
import qualified Control.Lens as L

import           Graphics.Vty

import           Action
import           Item
import           Mob
import           Misc

data Level = Level
  { _levelTiles :: !(Array Coord Tile)
  , _levelMobs  :: ![Mob]
  }
  deriving (Read, Show, Eq)

data Interaction
  = Passable
  | Impassable
  | Exit
  | Trigger !Action
  deriving (Read, Show, Eq, Ord)

data Tile = Tile
  { _tileSymbol      :: !Symbol
  , _tileInteraction :: !Interaction
  , _tileInventory   :: ![Item]
  } deriving (Read, Show, Eq)

-- For now, we just create a static boring level
-- for testing purposes
mkLevel :: Level
mkLevel = Level
  { _levelTiles = array ((0,0),(10,10))
                        [ ((x,y), passable) | x <- [0..maxX]
                                            , y <- [0..maxY] ]
  , _levelMobs  = []
  }
  where
  maxX = 10
  maxY = 10
  passable = Tile
    { _tileSymbol      = Symbol { _sChar = ' '
                                , _sAttr = Just $ defAttr `withBackColor` green }
    , _tileInteraction = Passable
    , _tileInventory   = []
    }

L.makeLenses ''Tile
L.makeLenses ''Level
