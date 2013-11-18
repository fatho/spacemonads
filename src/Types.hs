{-# LANGUAGE TemplateHaskell #-}
module Types 
  ( Ship (..)
  , Invader (..)
  , invaderType
  , InvaderType (..)
  , Swarm (..)
  , invaders
  , AABB (..)
  , topLeft, bottomRight
  , getAABB
  , Scene (..)
  , V2D
  , V2I
  , HasPosition (..)
  , HasSize (..)
  , HasBoundingBox (..)
  ) where

import Control.Lens
import Data.Monoid
import Linear

type V2D = V2 Double
type V2I = V2 Int

data InvaderType = Inv0 | Inv1 | Inv2 | Inv3 | Inv4 | Inv5
                   deriving (Show, Eq, Ord, Enum, Bounded)

data Ship    = Ship { _shipPos :: V2D, _shipSize :: V2D } deriving (Show)
data Invader = Invader { _invaderType :: InvaderType
                       , _invaderPos  :: V2D
                       } deriving (Show)
data Swarm   = Swarm 
                { _swarmTopLeft :: V2D
                , _invaders :: [Invader]
                } deriving (Show)

data AABB = AABB { _topLeft :: V2D, _bottomRight :: V2D } | AABBNothing deriving (Show, Eq)

data Scene = TitleScene | PauseScene 
           | GameScene { _playerShip :: Ship }

makeLenses ''Ship
makeLenses ''Invader
makeLenses ''Swarm
makeLenses ''AABB

class HasPosition a where
  position :: Lens' a V2D

class HasSize a where
  size :: Getter a V2D

class HasBoundingBox a where
  aabb :: Getter a AABB

instance HasPosition Ship where
  position = shipPos
instance HasPosition Invader where
  position = invaderPos
instance HasPosition Swarm where
  position = swarmTopLeft

instance HasSize Ship where
  size = shipSize
instance HasSize Invader where
  size = to $ \(Invader ty _) -> case ty of
    Inv0 -> V2 24 24
    Inv1 -> V2 40 32
    Inv2 -> V2 32 24
    Inv3 -> V2 48 24
    Inv4 -> V2 36 24
    Inv5 -> V2 48 24

instance Monoid AABB where
  mempty  = AABBNothing
  mappend = union

getAABB :: (HasPosition a, HasSize a) => Getter a AABB
getAABB = to mkAABB where
    mkAABB obj = 
      let h = obj ^. size ^/ 2 
      in AABB (obj ^. position ^-^ h) (obj ^. position ^+^ h)

union :: AABB -> AABB -> AABB
union AABBNothing AABBNothing = AABBNothing
union AABBNothing bb = bb
union bb AABBNothing = bb
union (AABB (V2 l1 t1) (V2 r1 b1)) (AABB (V2 l2 t2) (V2 r2 b2))
  = AABB (V2 (min l1 l2) (min t1 t2)) (V2 (max r1 r2) (max b1 b2))

translateAABB :: V2D -> AABB -> AABB
translateAABB v AABBNothing  = AABBNothing
translateAABB v (AABB v1 v2) = AABB (v ^+^ v1) (v ^+^ v2)

instance HasBoundingBox Ship where
  aabb = getAABB
instance HasBoundingBox Invader where
  aabb = getAABB
instance HasBoundingBox Swarm where
  aabb = to $ \swarm -> translateAABB (swarm ^. position) $
    case swarm ^. invaders ^. to (map (^. aabb)) of
      [] -> mempty 
      as -> foldl1 union as


