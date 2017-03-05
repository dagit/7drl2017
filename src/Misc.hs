{-# LANGUAGE TemplateHaskell #-}
module Misc
( Coord
, Symbol(..)
, sAttr
, sChar
) where

import Control.Lens

import Graphics.Vty

type Coord  = (Int, Int)
data Symbol = Symbol
  { _sAttr :: Maybe Attr
  , _sChar :: Char
  } deriving (Read, Show, Eq)

makeLenses ''Symbol
