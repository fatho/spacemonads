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
import Control.Monad.Random.Class
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

levelWire :: (Monad m, MonadFix m) => Int -> Int -> WireL m UI Scene
levelWire stage score = proc ui -> do
    rec
      remainingBullets <- stepWires . (pure () &&& delay []) -< activeBullets


      let currentBullets = map fst remainingBullets
      (newShip, newBullets) <- player -< (ui, currentBullets)

      let activeBullets = newBullets ++ map snd remainingBullets
    case newShip of
      Just  ship -> returnA -< GameScene ship currentBullets initialSwarm
      Nothing    -> inhibit (LevelOver Died 0) -< ()
  where
    initialSwarm = Swarm (V2 50 50) 
      [Invader (toEnum j) 
               (V2 (fromIntegral i * 64 + 32) 
                   (fromIntegral j * 32 + 32))
        | i <- [0..9], j <- [0..5]]


player :: Monad m => WireL m (UI, [Bullet]) (Maybe Ship, [WireL m () Bullet])
player = proc (ui, otherBullets) -> do
    newShip <- fly -< ui

    -- collision detection
    let
      shipAABB = newShip ^. aabb
      isAlienBullet (Bullet _ own) = case own of
        AlienBullet -> True
        PlayerBullet -> False
      died = or $ map (objectCollide shipAABB) (filter isAlienBullet otherBullets)

    newBullets <- fire -< (ui, newShip)

    if died
      then returnA -< (Nothing, newBullets)
      else returnA -< (Just newShip, newBullets)
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
          returnA -< [bullet bulletPos (-200) PlayerBullet]

      in try <|> pure []


isShooting :: Monad m => WireL m UI UI
isShooting = (asSoonAs . keyDown (CharKey ' ') &&& id) >>> arr snd >>> (once' --> cooldown >>> isShooting) where
  cooldown :: Monad m => WireL m a a
  cooldown = after 0.1


bullet :: (Monad m) => V2 Double -> Double -> BulletOwner -> WireL m a Bullet
bullet initialPos vspeed owner = Bullet <$> for 3.0 . integral initialPos . pure (V2 0 vspeed) <*> pure owner


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

--swarmWire :: (MonadRandom m) => WireG m Swarm Swarm
--swarmWire = 