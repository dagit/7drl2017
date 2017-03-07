module Action
( TriggerAction(..)
) where

import Data.Text

data TriggerAction
  = Trap Text Int -- ^ Message and damage caused by the trap
  deriving (Read, Show, Eq, Ord)

