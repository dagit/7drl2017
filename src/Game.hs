{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Game
( Game
, RenderState(..)
, rsImage
, rsVty
, mkRenderState
, GameState(..)
, gsPlayer
, gsLevel
, gsExit
, getVty
, getRS
, putRS
, modifyRS
, getGS
, putGS
, modifyGS
, mkGameState
, runGame
, exit
) where

import           Graphics.Vty
import           Control.Monad.State.Strict

import           Control.Lens

import           Level as L
import           Player

newtype Game a = Game (StateT (RenderState, GameState) IO a)
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadState (RenderState, GameState)
           )

data RenderState = RenderState
  { _rsImage :: !(Maybe Image)
  , _rsVty   :: !Vty
  }

data GameState = GameState
  { _gsPlayer :: !Player
  , _gsLevel  :: !L.Level
  , _gsExit   :: !Bool
  } deriving (Read, Show, Eq)

makeLenses ''GameState
makeLenses ''RenderState

mkGameState :: Player -> L.Level -> GameState
mkGameState p l = GameState
  { _gsPlayer = p
  , _gsLevel  = l
  , _gsExit   = False
  }

mkRenderState :: Vty -> RenderState
mkRenderState vty = RenderState
  { _rsImage = Nothing
  , _rsVty   = vty
  }

getVty :: Game Vty
getVty = (_rsVty . fst) <$> get

getRS :: Game RenderState
getRS = fst <$> get

putRS :: RenderState -> Game ()
putRS rs = modify (\(_,gs) -> (rs,gs))

modifyRS :: (RenderState -> RenderState) -> Game ()
modifyRS f = modify (\(rs,gs) -> (f $! rs, gs))

getGS :: Game GameState
getGS = snd <$> get

putGS :: GameState -> Game ()
putGS gs = modify (\(rs,_) -> (rs,gs))

modifyGS :: (GameState -> GameState) -> Game ()
modifyGS f = modify (\(rs,gs) -> (rs, f $! gs))

exit :: Game ()
exit = _2.gsExit .= True

-- | Note: This is lossy with respect to the rendering state.
-- The intention is that the render state can always be reconstructed
-- and that this function is for starting from some initial state and
-- running the game to completetion.
runGame :: Game a -> RenderState -> GameState -> IO (a, GameState)
runGame (Game g) rs gs = do
  let m = runStateT g (rs, gs)
  (a, (_, s)) <- m
  return $! (a, s)
