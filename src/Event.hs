module Event
( Event(..)
) where

data Event
  = TryMovePlayerBy Int Int
  | Exit
  | Redraw
  deriving (Read, Show, Eq, Ord)
