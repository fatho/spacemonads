{-# LANGUAGE Arrows, StandaloneDeriving #-}
module Logic where

import Control.Applicative
import Control.Arrow
import Control.Wire
import Control.Wire.Unsafe.Event
import FRP.Netwire

import Control.Lens
import Control.Lens.TH
import Control.Monad.Random.Class
import Prelude hiding ((.), id, until)
import Graphics.UI.GLFW as GLFW
import Linear

import Debug.Trace

import Data.Set as Set

import Framework
import Types

type TimeT = Timed NominalDiffTime ()
type WireG = Wire TimeT ()

type SceneWire = WireG IO UI (Scene, Event EvtScene)

deriving instance Show a => Show (Event a)

data EvtScene = Switch (SceneWire -> SceneWire)
isSwitchEvent :: EvtScene -> Bool
isSwitchEvent (Switch _) = True
isSwitchEvent _          = False

traceId :: Show a => a -> a
traceId x = traceShow x x

mainWire :: WireG IO UI Scene
mainWire = proc ui -> do
  rec
    (scene, sceneEvent) <- krSwitch titleWire . (id *** delay NoEvent) -< (ui, switchEvent)
    switchEvent <- fmap (\(Switch sw) -> sw) <$> filterE isSwitchEvent -< sceneEvent

  returnA -< scene

titleWire :: SceneWire
titleWire = (pure TitleScene &&& fmap selWire <$> multiKeyEvent [SpecialKey ESC, SpecialKey ENTER] keyPress) where
  selWire (SpecialKey ESC) = Switch $ (const $ inhibit mempty)
  selWire (SpecialKey ENTER) = Switch (const gameWire)

pauseWire :: SceneWire -> SceneWire
pauseWire orig = (pure PauseScene &&& fmap selWire <$> multiKeyEvent [SpecialKey ESC, SpecialKey ENTER] keyPress)  where
  selWire (SpecialKey ESC) = Switch (const titleWire)
  selWire (SpecialKey ENTER) = Switch (const orig)

gameWire :: SceneWire
gameWire = (go &&& events) where
  go = GameScene <$> player -- pure (GameScene $ Ship (V2 400 580) (V2 52 32))
  events = fmap (const $ Switch pauseWire) <$> keyPress (SpecialKey ESC)


player :: Monad m => WireG m UI Ship
player = proc ui -> do
    x <- integralWith clamp 400 . (input &&& id) -< ui
    returnA -< Ship (V2 x 580) (V2 52 32)
  where
    clamp _ x
      | x < 26  = 26
      | x > 774 = 774
      | otherwise = x
    input :: Monad m => WireG m UI Double
    input =  -200 . whenKeyDown (SpecialKey LEFT)
         <|> 200 . whenKeyDown (SpecialKey RIGHT)
         <|> 0




-- * EVENT WIRES
-------------------------------------------------------------------------------

multiKeyEvent :: (Monad m, Monoid e) => [Key] -> (Key -> Wire s e m UI (Event Key)) -> Wire s e m UI (Event Key)
multiKeyEvent ks f = foldl1 (<&) $ Prelude.map f ks

keyDown :: (Monad m, Monoid e) => Key -> Wire s e m UI (Event Key)
keyDown k = now . whenKeyDown k <|> never

keyUp :: (Monad m, Monoid e) => Key -> Wire s e m UI (Event Key)
keyUp k = now . whenKeyUp k <|> never

whenKeyDown :: (Monad m, Monoid e) => Key -> Wire s e m UI Key
whenKeyDown k = pure k . when (^. pressedKeys . to (member k) )

whenKeyUp :: (Monad m, Monoid e) => Key -> Wire s e m UI Key
whenKeyUp k = pure k . unless (^. pressedKeys . to (member k) )

keyPress :: (Monoid e) => Key -> Wire s e m UI (Event Key)
keyPress k = 
    mkPureN $ \ui ->
        if ui ^. pressedKeys ^. to (member k)
          then (Right NoEvent, waitRelease)
          else (Right NoEvent, keyPress k)
  where
    waitRelease =
        mkPureN $ \ui ->
          if ui ^. pressedKeys ^. to (member k)
            then (Right NoEvent, waitRelease)
            else (Right (Event k), keyPress k)

-- * WIRE COMBINATORS
-------------------------------------------------------------------------------

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