module Main where

import Graphics.UI.GLFW 
import Graphics.GL.Low
import Control.Applicative

import Control.Broccoli
import Glue
import Util

data Scene = Scene
data Sound = Sound

setupGL :: IO ()
setupGL = do
  --putStrLn "setupGL"
  return ()

renderScene :: Scene -> IO ()
renderScene _ = do
  --putStrLn "renderScene"
  return ()

main :: IO ()
main = runGlfw 640 480 "Broccoli" setupGL renderScene $ \glfw onBoot time -> do
  let jaxis0 = glfwJoystickAxes ((glfwJoysticks glfw) !! 0)
  let jbut0 = glfwJoystickButtons ((glfwJoysticks glfw) !! 0)
  let joy0 = glfwJoystickPresent ((glfwJoysticks glfw) !! 0)
  let keyboard = glfwKey glfw
  let mouse = glfwCursorPos glfw
  let clickRelease = glfwMouseButton glfw
  let (scene, _) = program onBoot time
  output (const print) (snapshot_ onBoot joy0)
  return (scene, glfwClose glfw)

program :: E () -> X Time -> (X Scene, E Sound)
program onBoot time = (scene, never) where
  scene = pure Scene

{-
  { glfwKey :: E (Key, Int, KeyState, ModifierKeys)
  , glfwChar :: E Char
  , glfwCursorPos :: X (Double,Double)
  , glfwMouseButton :: E (MouseButton, MouseButtonState, ModifierKeys)
-}
