module Mob
( Mob(..)
) where

import Misc

data Mob = Mob
  { _mobSymbol :: Symbol
  , _mobHP     :: Int
  } deriving (Read, Show, Eq)
