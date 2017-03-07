module Event
( Event(..)
) where

data Event
  = TryMovePlayerBy Int Int
  | TryUpStairs
  | TryDownStairs
  | Exit
  | Redraw
  deriving (Read, Show, Eq, Ord)
