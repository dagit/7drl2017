{-# LANGUAGE TemplateHaskell #-}
module Item
( Item(..)
) where

import Control.Lens

import Misc

data Item = Item
  { _itemSymbol      :: Symbol
  , _itemDescription :: String
  } deriving (Read, Show, Eq)

makeLenses ''Item
