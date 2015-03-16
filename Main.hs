module Main where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL.Low
import qualified Data.Vector.Storable as V

import Control.Broccoli
import Glue

data Scene = Scene

setupGL :: IO ()
setupGL = do
  putStrLn "setupGL"

renderScene :: Scene -> IO ()
renderScene _ = do
  putStrLn "renderScene"

diff :: Eq a => a -> a -> Maybe (a,a)
diff a b = if a == b then Nothing else Just (a,b)

main :: IO ()
main = runGlfw 640 480 "Broccoli" $ \glins mainThread onBoot time -> do
  output (const print) (onKey glins)
  output (const print) (onChar glins)
  output (const print) (edge diff (onCursorPos glins))
  output (const print) (onMouseButton glins)
  output (\_ _ -> mainThread setupGL) onBoot
  output (\_ _ -> mainThread (renderScene Scene)) (onVsync glins)
  return (onClose glins)

setup = do
  -- establish a VAO
  vao <- newVAO
  bindVAO vao
  -- load shader program
  vsource <- readFile "hello.vert"
  fsource <- readFile "hello.frag"
  prog <- newProgram vsource fsource
  useProgram prog
  -- load vertex data: three 2D vertex positions
  let blob = V.fromList
        [ -0.5, -0.5
        ,    0,  0.5
        ,  0.5, -0.5 ] :: V.Vector Float
  vbo <- newVBO blob StaticDraw
  bindVBO vbo
  -- connect program to vertex data via the VAO
  setVertexLayout [Attrib "position" 2 (GLScalarAttrib (GLFloat Single))]
  return (vao, prog)

draw vao prog = do
  clearColorBuffer (0,0,0)
  bindVAO vao
  useProgram prog
  drawTriangles 3
