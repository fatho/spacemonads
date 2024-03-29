{-# LANGUAGE Arrows, StandaloneDeriving #-}
module Logic
  ( mainWire
  ) where

import Control.Applicative
import Control.Arrow
import Control.Wire
import Control.Wire.Unsafe.Event
import FRP.Netwire

import Control.Lens
import Control.Lens.TH
import Control.Monad.Random
import Control.Monad.Fix
import Prelude hiding ((.), id, until)
import Graphics.UI.GLFW as GLFW (Key(..), SpecialKey (..))
import Linear

import Debug.Trace

import qualified Data.Set as Set

import Framework
import Types

type TimeT = Timed NominalDiffTime ()
type WireG = Wire TimeT LevelOver
type WireL = Wire TimeT LevelOver

type SceneWire = WireG IO UI (Scene, Event EvtScene)

deriving instance Show a => Show (Event a)

data EvtScene = Switch (SceneWire -> SceneWire)
isSwitchEvent :: EvtScene -> Bool
isSwitchEvent (Switch _) = True
isSwitchEvent _          = False

data EndReason = Cleared | Died  deriving (Show)
data LevelOver = LevelOver EndReason Int deriving (Show)

instance Monoid LevelOver where
  mempty = LevelOver Died 0
  mappend (LevelOver Died n) (LevelOver Died m) = LevelOver Died (max n m)
  mappend (LevelOver _ n) (LevelOver _ m) = LevelOver Cleared (max n m)

traceId :: Show a => a -> a
traceId x = traceShow x x

-- * constants
-------------------------------------------------------------------------------
playerShipSize :: V2 Double
playerShipSize = V2 56 32

-- * wires
-------------------------------------------------------------------------------

mainWire :: WireG IO UI Scene
mainWire = proc ui -> do
  rec
    -- ... and feed it into the switch in the past
    (scene, sceneEvent) <- krSwitch titleWire . (id *** delay NoEvent) -< (ui, switchEvent)
    -- filter scene switch events from the result of the scene wire ...
    switchEvent <- fmap (\(Switch sw) -> sw) <$> filterE isSwitchEvent -< sceneEvent

  returnA -< scene

titleWire :: SceneWire
titleWire = (pure TitleScene &&& events) where
  -- handle navigation keys
  events = fmap selWire <$> multiKeyEvent [SpecialKey ESC, SpecialKey ENTER] keyPress
  selWire (SpecialKey ESC) = Switch $ (const $ inhibit mempty)
  selWire (SpecialKey ENTER) = Switch (const gameWire)

pauseWire :: SceneWire -> SceneWire
pauseWire orig = (pure PauseScene &&& events)  where
  -- handle navigation keys
  events = fmap selWire <$> multiKeyEvent [SpecialKey ESC, SpecialKey ENTER] keyPress
  selWire (SpecialKey ESC) = Switch (const titleWire)
  selWire (SpecialKey ENTER) = Switch (const orig)

gameWire :: SceneWire
gameWire = (go 1 0 &&& events) >>> merge where
  -- 
  go stage score = runLevel stage (levelWire stage score) -- levelWire &&& pure NoEvent --
  -- level switcher
  runLevel n lvl = mkGen $ \t ui -> do
        (r, w) <- stepWire lvl t (Right ui)
        case r of
          Right scene -> return $ (Right (scene, NoEvent), runLevel n w)
          Left (LevelOver Died _) -> 
            return $ (Right (TitleScene, Event $ Switch (const titleWire)), runLevel n w)
          Left (LevelOver Cleared score) -> 
            return $ (Right (TitleScene, NoEvent), go (n+1) score)
  
  merge :: Wire s e m ((Scene, Event EvtScene), Event EvtScene) (Scene, Event EvtScene)
  merge = mkPure_ $ \((s, ev1), ev2) -> Right (s, mergeL ev1 ev2)
  -- handle navigation keys
  events = fmap (const $ Switch pauseWire) <$> keyPress (SpecialKey ESC)

levelWire :: (MonadRandom m, MonadFix m) => Int -> Int -> WireL m UI Scene
levelWire stage score = proc ui -> do
    rec
      activeBullets <- stepWires . (pure () &&& delay []) -< nextBullets
      let bulletEntities = map fst activeBullets

      activeSwarm <- stepSwarm (recip $ fromIntegral stage) . delay initialSwarm -< nextSwarm
      newAlienBullets <- alienShots -< activeSwarm

      (ship, newBullets) <- player -< (ui, bulletEntities)
      (nextSwarm, remainingBullets) <- collide -< (activeSwarm, activeBullets)

      let nextBullets = newBullets ++ newAlienBullets ++ map snd remainingBullets
    case ship of
      Nothing    -> inhibit (LevelOver Died 0) -< ()
      Just  ship -> 
        if null (nextSwarm ^. invaders)
          then inhibit (LevelOver Cleared 0) -< ()
          else returnA -< GameScene ship bulletEntities nextSwarm
  where
    initialSwarm = Swarm 
      (V2 50 50) 
      [Invader (toEnum (2 * (j `div` 2))) 
               (V2 (fromIntegral i * 64 + 16) 
                   (fromIntegral j * 32 + 16))
        | i <- [0..9], j <- [0..5]]
      SwarmRight
      SwarmA


player :: Monad m => WireL m (UI, [Bullet]) (Maybe Ship, [WireL m () Bullet])
player = proc (ui, otherBullets) -> do
    newShip <- fly -< ui

    -- collision detection
    let
      shipAABB = newShip ^. aabb
      playerBulletActive = not . null $ filter (not . isAlienBullet) otherBullets
      died = or $ map (\b -> aabbContains shipAABB (b ^. position)) (filter isAlienBullet otherBullets)

    newBullets <- fire -< (ui, newShip)

    let
      newBullet' = if playerBulletActive then [] else newBullets

    if died
      then returnA -< (Nothing, [])
      else returnA -< (Just newShip, newBullet')
  where
    clamp ui x
        | x < leftBound = leftBound
        | x > rightBound = rightBound
        | otherwise = x
      where
        leftBound = playerShipSize ^. _x / 2
        rightBound = ui ^. windowSize . _x . to fromIntegral - playerShipSize ^. _x / 2
    input = liftA2 (+)
      (-200 . whenKeyDown (SpecialKey LEFT) <|> 0)
      (200 . whenKeyDown (SpecialKey RIGHT) <|> 0)

    fly = proc ui -> do
      let (V2 w h) = ui ^. windowSize
      x <- integralWith clamp 400 . (input &&& id) -< ui
      returnA -< Ship (V2 x (fromIntegral h - playerShipSize ^. _y / 2 - 4)) playerShipSize

    fire = 
      let 
        try = proc (ui, ship) -> do
          isShooting -< ui
          let bulletPos = ship ^. position - V2 0 (playerShipSize ^. _y)
          returnA -< [bullet bulletPos (-300) PlayerBullet]

      in try <|> pure []


isShooting :: Monad m => WireL m UI UI
isShooting = (asSoonAs . keyDown (CharKey ' ') &&& id) >>> arr snd >>> (once' --> cooldown >>> isShooting) where
  cooldown :: Monad m => WireL m UI UI
  cooldown = (asSoonAs . keyUp (CharKey ' ') . after 0.1 &&& id) >>> arr snd


bullet :: (Monad m, Monoid e) => V2 Double -> Double -> BulletOwner -> Wire TimeT e m a Bullet
bullet initialPos vspeed owner = Bullet <$> dieWhenOutside . integral initialPos . pure (V2 0 vspeed) <*> pure owner
  where
    dieWhenOutside = mkSFN $ \pos@(V2 x y) ->
      let isInside = x >= 0 && y >= 0 && x <= 800 && y <= 600
      in (pos, if isInside then dieWhenOutside else inhibit mempty)

stepSwarm :: (Monad m) => Double -> WireL m Swarm Swarm
stepSwarm t = id . for (realToFrac t) --> once' . step --> stepSwarm t where
  step = proc (Swarm pos invs move anim) -> do
    let 
      (V2 x y) = pos
      (w,h)    = swarmSize invs
      (delta, next) = case move of
        SwarmRight
          | x + w < 800 -> (V2 10 0, SwarmRight)
          | otherwise        -> (0, SwarmDown 2 SwarmLeft)
        SwarmLeft   
          | x - 24 > 0      -> (V2 (-10) 0, SwarmLeft)
          | otherwise        -> (V2 0 10, SwarmDown 2 SwarmRight)
        SwarmDown 0 n -> (V2 (sgn' n * 10) 0, n)
        SwarmDown i n -> (V2 0 10, SwarmDown (i-1) n)
      sgn' SwarmLeft = -1
      sgn' _         = 1

    returnA -< Swarm (pos ^+^ delta) invs next (animate anim)
  animate SwarmA = SwarmB
  animate SwarmB = SwarmA
  swarmSize invs = (maxXInv + 48, maxYInv + 32) where
    maxXInv = maximum $ map (^. position . _x) invs
    maxYInv = maximum $ map (^. position . _y) invs

alienShots :: (MonadRandom m) => WireL m Swarm [WireL m () Bullet]
alienShots = spawnBullet . wackelkontaktM 0.01 <|> pure [] where
  spawnBullet = mkGen_ $ \swarm -> do
    let invs = swarm ^. invaders
    i <- getRandomR (0, length invs - 1)
    let pos = swarm ^. invaders . to (!!i) . position
    return $ Right [bullet (pos ^+^ (swarm ^. position)) 200 AlienBullet]
    

collide :: (Monad m, MonadFix m) => WireL m (Swarm, [(Bullet, WireL m a Bullet)]) (Swarm, [(Bullet, WireL m a Bullet)])
collide = proc (swarm, bullets) -> do
      let 
        invOff = swarm ^. position
        (remainingBullets, newInvs) = collideAll invOff bullets (swarm ^. invaders)

      returnA -< (swarm { _invaders = newInvs }, remainingBullets )
  where
    collideAll :: V2D -> [(Bullet, WireL m a Bullet)] -> [Invader] -> ([(Bullet, WireL m a Bullet)], [Invader])
    collideAll off []     invs = ([], invs)
    collideAll off (b:bs) invs = 
      let
        (b', invs') = collide1 off b invs
        (bs', invs'') = collideAll off bs invs'
      in (b' ++ bs', invs'')


    collide1 :: V2D -> (Bullet, WireL m a Bullet) -> [Invader] -> ([(Bullet, WireL m a Bullet)],[Invader])
    collide1 invOff b [] = ([b], [])
    collide1 invOff b (i:is) 
      | (not $ isAlienBullet $ fst b)
        && aabbContains (translateAABB invOff (i ^. aabb)) (b ^. to fst . position)
                  = ([], is)
      | otherwise = let (bs, is') = collide1 invOff b is in (bs, i:is')


-- * EVENT WIRES
-------------------------------------------------------------------------------

multiKeyEvent :: (Monad m, Monoid e) => [Key] -> (Key -> Wire s e m UI (Event Key)) -> Wire s e m UI (Event Key)
multiKeyEvent ks f = foldl1 (<&) $ Prelude.map f ks

keyDown :: (Monad m, Monoid e) => Key -> Wire s e m UI (Event Key)
keyDown k = now . pure k . whenKeyDown k <|> never

keyUp :: (Monad m, Monoid e) => Key -> Wire s e m UI (Event Key)
keyUp k = now . pure k . whenKeyUp k <|> never

whenKeyDown :: (Monad m, Monoid e) => Key -> Wire s e m UI UI
whenKeyDown k = when (^. pressedKeys . to (Set.member k) )

whenKeyUp :: (Monad m, Monoid e) => Key -> Wire s e m UI UI
whenKeyUp k = unless (^. pressedKeys . to (Set.member k) )

keyPress :: (Monoid e) => Key -> Wire s e m UI (Event Key)
keyPress k = 
    mkPureN $ \ui ->
        if ui ^. pressedKeys ^. to (Set.member k)
          then (Right NoEvent, waitRelease)
          else (Right NoEvent, keyPress k)
  where
    waitRelease =
        mkPureN $ \ui ->
          if ui ^. pressedKeys ^. to (Set.member k)
            then (Right NoEvent, waitRelease)
            else (Right (Event k), keyPress k)

-- * WIRE COMBINATORS
-------------------------------------------------------------------------------

once' :: (Monoid e) => Wire s e m a a
once' = mkPureN $ \x -> (Right x, inhibit mempty)


stepWires :: (Monad m, Monoid s) => Wire s e m (a, [Wire s e m a b]) [(b, Wire s e m a b)]
stepWires = mkGen $ \dt (x, ws) -> do
  results <- mapM (\w -> stepWire w dt (Right x)) ws
  return $ (Right [(x', w') | (Right x', w') <- results], stepWires)


-- | Whenever the given wire inhibits, a new wire is constructed using
-- the given function.
--
-- * Depends: like currently active wire.
--
-- * Time: switching restarts time.

switchBy ::
    (Monad m, Monoid s)
    => (e -> Maybe (Wire s e m a b))  -- ^ Wire selection function.
    -> Wire s e m a b          -- ^ Initial wire.
    -> Wire s e m a b
switchBy new w0 =
    mkGen $ \dt x' ->
        let select w' = do
                (mx, w) <- stepWire w' dt (Right x')
                case mx of
                  Left ex -> maybe (return (Left ex, switchBy new w)) select (new ex)
                  Right x -> return (Right x, switchBy new w)
        in select w0


switchByOnce ::
    (Monad m, Monoid s)
    => (e' -> Wire s e' m a b -> Wire s e m a b)  -- ^ Wire selection function.
    -> Wire s e' m a b          -- ^ Initial wire.
    -> Wire s e m a b
switchByOnce new w0 =
    mkGen $ \dt x' ->
        let select w' = do
                (mx, w) <- stepWire w' dt (Right x')
                case mx of
                  Left ex -> stepWire (new ex w) dt (Right x')
                  Right x -> return (Right x, switchByOnce new w)
        in select w0

untilWith :: (Monoid e, Monad m) => b -> Wire s e m (Event a) b
untilWith v = (pure v &&& id) >>> until

wackelkontaktM :: (MonadRandom m, Monoid e)
               => Double  -- ^ Occurrence probability.
               -> Wire s e m a a
wackelkontaktM p =
    mkGen_ $ \x -> do
        e <- getRandom
        return (if (e < p) then Right x else Left mempty)