{-# LANGUAGE TemplateHaskell #-}
module Types 
  ( Ship (..)
  , Invader (..)
  , invaderType
  , InvaderType (..)
  , Swarm (..)
  , SwarmMovement (..)
  , SwarmAnim (..)
  , invaders
  , swarmMove
  , swarmAnim
  , Bullet (..)
  , BulletOwner (..)
  , isAlienBullet
  , AABB (..)
  , topLeft, bottomRight
  , getAABB
  , translateAABB
  , Scene (..)
  , V2D
  , V2I
  , HasPosition (..)
  , HasSize (..)
  , HasBoundingBox (..)
  , aabbCollide
  , aabbContains
  , objectCollide
  ) where

import Control.Lens
import Data.Monoid
import Linear

type V2D = V2 Double
type V2I = V2 Int

data InvaderType = Inv0 | Inv1 | Inv2 | Inv3 | Inv4 | Inv5
                   deriving (Show, Eq, Ord, Enum, Bounded)

data Ship    = Ship 
                { _shipPos :: V2D
                , _shipSize :: V2D
                } deriving (Show)
data Invader = Invader 
                { _invaderType :: InvaderType
                , _invaderPos  :: V2D
                } deriving (Show)
data Swarm   = Swarm 
                { _swarmTopLeft :: V2D
                , _invaders :: [Invader]
                , _swarmMove :: SwarmMovement
                , _swarmAnim :: SwarmAnim
                } deriving (Show)
data SwarmMovement = SwarmLeft | SwarmRight | SwarmDown Int SwarmMovement deriving (Show)
data SwarmAnim     = SwarmA | SwarmB deriving (Show)
data Bullet  = Bullet 
                { _bulletPos :: V2D
                , _bulletOwner :: BulletOwner
                } deriving (Show)
data BulletOwner = PlayerBullet 
                 | AlienBullet deriving (Show)

data AABB = AABB { _topLeft :: V2D, _bottomRight :: V2D } | AABBNothing deriving (Show, Eq)

data Scene = TitleScene | PauseScene 
           | GameScene 
              { _playerShip :: Ship 
              , _bullets :: [Bullet]
              , _swarm :: Swarm
              }

makeLenses ''Ship
makeLenses ''Invader
makeLenses ''Swarm
makeLenses ''Bullet
makeLenses ''AABB

isAlienBullet :: Bullet -> Bool
isAlienBullet (Bullet _ own) = case own of
  AlienBullet -> True
  PlayerBullet -> False

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
instance HasPosition Bullet where
  position = bulletPos

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


aabbCollide :: AABB -> AABB -> Bool
aabbCollide (AABB p1 s1) (AABB p2 s2) = delta ^. _x < size ^. _x && delta ^. _y < size ^. _y where
  delta = fmap abs $ c1 ^-^ c2
  size  = hs1 ^+^ hs2
  hs1 = s1 ^/ 2
  hs2 = s2 ^/ 2
  c1  = p1 ^+^ hs1
  c2  = p2 ^+^ hs2

aabbContains :: AABB -> V2D -> Bool
aabbContains (AABB tl br) p = cx && cy where
  cx = (p ^. _x) `between` (tl ^. _x, br ^. _x)
  cy = (p ^. _y) `between` (tl ^. _y, br ^. _y)
  between x (low, high) = x >= low && x <= high

objectCollide :: (HasBoundingBox a, HasBoundingBox b) => a -> b -> Bool
objectCollide x y = aabbCollide (x ^. aabb) (y ^. aabb)


instance HasBoundingBox Ship where
  aabb = getAABB
instance HasBoundingBox Invader where
  aabb = getAABB
instance HasBoundingBox Swarm where
  aabb = to $ \swarm -> translateAABB (swarm ^. position) $
    case swarm ^. invaders ^. to (map (^. aabb)) of
      [] -> mempty 
      as -> foldl1 union as
instance HasBoundingBox Bullet where
  aabb = to $ \(Bullet pos _) -> AABB pos pos
instance HasBoundingBox AABB where
  aabb = id

