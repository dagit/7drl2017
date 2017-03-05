{-# LANGUAGE TemplateHaskell #-}
module Level
( Level(..)
, Tile(..)
, Interaction(..)
, mkEmptyLevel
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
mkEmptyLevel :: (Int,Int) -> (Int, Int) -> Level
mkEmptyLevel (minX,minY) (maxX, maxY) = Level
  { _levelTiles = array ((minX,minY),(maxX,maxY))
                        [ ((x,y), tile (x,y)) | x <- [minX..maxX]
                                              , y <- [minY..maxY] ]
  , _levelMobs  = []
  }
  where
  tile (x,y) | x == minX     ||
               y == minY     ||
               x == (maxX-1) ||
               y == (maxY-1) = wallTile
             | otherwise     = emptyTile

emptyTile :: Tile
emptyTile = Tile
  { _tileSymbol = Symbol
    { _sChar = '.'
    , _sAttr = Just $ defAttr `withBackColor` black
    }
  , _tileInteraction = Passable
  , _tileInventory = []
  }

wallTile :: Tile
wallTile = Tile
  { _tileSymbol = Symbol
    { _sChar = '#'
    , _sAttr = Just $ defAttr `withBackColor` black
    }
  , _tileInteraction = Impassable
  , _tileInventory = []
  }

L.makeLenses ''Tile
L.makeLenses ''Level
