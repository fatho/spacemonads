{-# LANGUAGE StandaloneDeriving, TemplateHaskell, Arrows #-}

module Main (main) where

import Graphics.UI.GLFW as GLFW

import Framework
import Game

import Linear

rd :: V2 Double -> V2 Double
rd v =
  let
    distortion = 0.1
    cc = v - 0.5
    dist = dot cc cc * distortion
  in v ^+^ cc ^* ((1.0 + dist) * dist)

main = setupGLFW "Space Monads" 800 600
    >>= runSpaceMonads
    >> GLFW.terminate