module Main where

import Graphics.UI.GLFW 
import Graphics.GL.Low
import Control.Applicative

import Control.Broccoli
import Glue

data Scene = Scene

setupGL :: IO ()
setupGL = do
  --putStrLn "setupGL"
  return ()

renderScene :: Scene -> IO ()
renderScene _ = do
  --putStrLn "renderScene"
  return ()

diff :: Eq a => a -> a -> Maybe (a,a)
diff a b = if a == b then Nothing else Just (a,b)

main :: IO ()
main = runGlfw 640 480 "Broccoli" setupGL renderScene $ \glfw onBoot time -> do
  output (const print) (glfwKey glfw)
  output (const print) (glfwChar glfw)
  output (const print) (edge diff (glfwCursorPos glfw))
  output (const print) (glfwMouseButton glfw)
  output (const print) (edge diff (glfwJoystickAxes ((glfwJoysticks glfw) !! 0)))
  let scene = pure Scene
  return (scene, glfwClose glfw)

