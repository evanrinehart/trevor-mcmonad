module Glue (
  GlfwInputs(..),
  GlfwJoystickInput(..),
  GlOut(..),
  execInMainThread,
  runGlfw
) where

import System.IO
import System.Exit
import Data.IORef
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad (when, forever)
import Control.Concurrent
import Control.Applicative

import Graphics.UI.GLFW 
import Control.Broccoli

data GlfwInputs = GlfwInputs
  { glfwKeyEvent :: E (Key, Int, KeyState, ModifierKeys)
  , glfwCharEvent :: E Char
  , glfwCursorPos :: X (Double,Double)
  , glfwMouseButtonEvent :: E (MouseButton, MouseButtonState, ModifierKeys)
  , glfwScrollEvent :: E (Double,Double)
  , glfwJoysticks :: Vector GlfwJoystickInput -- 16
  , glfwFramebufferSizeEvent :: E (Int,Int)
  , glfwVsync :: E ()
  , glfwWindowCloseEvent :: E ()
  }

data GlfwJoystickInput = GlfwJoystickInput
  { glfwJoystickAppearEvent :: E String -- name of joystick
  , glfwJoystickLeaveEvent :: E ()
  , glfwJoystickAxes :: X [Double]
  , glfwJoystickButtonEvent :: E (Int, JoystickButtonState) }

nullJoystick :: GlfwJoystickInput
nullJoystick = GlfwJoystickInput never never (pure [0,0]) never

errorCb :: Error -> String -> IO ()
errorCb err msg = do
  hPutStrLn stderr (show err ++ "(" ++ msg ++ ")")
  exitFailure

newtype GlOut = GlOut (MVar (IO ()))
execInMainThread :: GlOut -> IO () -> IO ()
execInMainThread (GlOut mv) io = putMVar mv io

runGlfw :: Int
        -> Int
        -> String
        -> (GlfwInputs -> GlOut -> E Boot -> X Time -> Setup (E ()))
        -> IO ()
runGlfw winW winH title setup = do
  setErrorCallback (Just errorCb)
  status <- Graphics.UI.GLFW.init
  when (status == False) $ do
    pollEvents
    exitFailure
  windowHint (WindowHint'ContextVersionMajor 3)
  windowHint (WindowHint'ContextVersionMinor 2)
  windowHint (WindowHint'OpenGLForwardCompat True)
  windowHint (WindowHint'OpenGLProfile OpenGLProfile'Core)
  mwin <- createWindow winW winH title Nothing Nothing
  case mwin of
    Nothing -> do
      pollEvents
      exitFailure
    Just win -> do
      windowClosingRef <- newIORef False
      pleaseExit <- newIORef False
      makeContextCurrent (Just win)
      swapInterval 1
      initialMouse <- return (0,0) --get from glfw
      windowMV <- newEmptyMVar
      keyMV <- newEmptyMVar
      charMV <- newEmptyMVar
      mouseMV <- newEmptyMVar
      clickMV <- newEmptyMVar
      vsyncMV <- newEmptyMVar
      closeMV <- newEmptyMVar
      graphicsMV <- newEmptyMVar
      setWindowSizeCallback  win (Just (sizeCb (putMVar windowMV)))
      setKeyCallback         win (Just (keyCb (putMVar keyMV)))
      setCharCallback        win (Just (charCb (putMVar charMV)))
      setMouseButtonCallback win (Just (clickCb (putMVar clickMV)))
      setCursorPosCallback   win (Just (mouseCb (putMVar mouseMV)))
      forkIO $ do
        runProgram $ \onBoot time -> do
          (mouse, setMouse) <- newX initialMouse
          (onClick, click) <- newE
          (onChar, typeChar) <- newE
          (onKey, doKey) <- newE
          (onResize, resize) <- newE
          (onVsync, vsync) <- newE
          (onClose, close) <- newE
          input (forever (takeMVar mouseMV >>= setMouse))
          input (forever (takeMVar clickMV >>= click))
          input (forever (takeMVar charMV >>= typeChar))
          input (forever (takeMVar keyMV >>= doKey))
          input (forever (takeMVar windowMV >>= resize))
          input (forever (takeMVar vsyncMV >>= vsync))
          input (forever (takeMVar closeMV >>= close))
          let joysticks = V.replicate 16 nullJoystick
          let glfwIns = GlfwInputs onKey onChar mouse onClick never
                  joysticks onResize onVsync onClose
          setup glfwIns (GlOut graphicsMV) onBoot time
        writeIORef pleaseExit True
      untilM (readIORef pleaseExit) $ do
        pollEvents
        putMVar vsyncMV ()
        glAction <- takeMVar graphicsMV
        glAction
        swapBuffers win
        flag <- readIORef windowClosingRef
        shouldClose <- windowShouldClose win
        when (shouldClose && flag == False) $ do
          putMVar closeMV ()
          writeIORef windowClosingRef True
      terminate
      
sizeCb :: ((Int,Int) -> IO ()) -> Window -> Int -> Int -> IO ()
sizeCb go _ w h = go (w,h)

keyCb :: ((Key,Int,KeyState,ModifierKeys) -> IO ())
      -> Window
      -> Key
      -> Int
      -> KeyState
      -> ModifierKeys
      -> IO ()
keyCb go _ key scancode state mods = go (key,scancode,state,mods)

charCb :: (Char -> IO ()) -> Window -> Char -> IO ()
charCb go _ c = go c

clickCb :: ((MouseButton,MouseButtonState,ModifierKeys) -> IO ())
        -> Window
        -> MouseButton
        -> MouseButtonState
        -> ModifierKeys
        -> IO ()
clickCb go _ button state mods = go (button,state,mods)

mouseCb :: ((Double,Double) -> IO ()) -> Window -> Double -> Double -> IO ()
mouseCb go _ x y = go (x,y)


untilM :: IO Bool -> IO a -> IO ()
untilM check action = do
  y <- check
  if y then action >> untilM check action else return ()
