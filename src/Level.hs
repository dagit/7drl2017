{-# LANGUAGE TemplateHaskell #-}
module Level
( Level(..)
, Tile(..)
, Interaction(..)
, mkEmptyLevel
, tileSymbol
, tileInteraction
, tileInventory
, isExit
, levelMobs
, levelTiles
, emptyTile
, wallTile
, upStairs
, downStairs
) where

import           Data.Array
import           Control.Lens hiding (Level)

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
  | UpStairs
  | DownStairs
  | Trigger !TriggerAction
  deriving (Read, Show, Eq, Ord)

isExit :: Interaction -> Bool
isExit UpStairs   = True
isExit DownStairs = True
isExit _          = False

data Tile = Tile
  { _tileSymbol      :: !Symbol
  , _tileInteraction :: !Interaction
  , _tileInventory   :: ![Item]
  } deriving (Read, Show, Eq)

makeLenses ''Tile
makeLenses ''Level

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
wallTile = emptyTile & (tileSymbol.sChar) .~ '#'
                     & tileInteraction    .~ Impassable

upStairs :: Tile
upStairs = emptyTile & (tileSymbol.sChar) .~ '>'
                     & tileInteraction    .~ UpStairs

downStairs :: Tile
downStairs = emptyTile & (tileSymbol.sChar) .~ '<'
                       & tileInteraction    .~ DownStairs
