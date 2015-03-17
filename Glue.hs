module Glue (
  GlfwInputs(..),
  GlfwJoystickInput(..),
  runGlfw
) where

import System.IO
import System.Exit
import Data.IORef
import Data.Vector (Vector)
import qualified Data.Vector as V
import Control.Monad (when, forever, mapM, forM, forM_, replicateM)
import Control.Concurrent
import Control.Applicative
import Data.Maybe

import Graphics.UI.GLFW 
import Control.Broccoli

data GlfwInputs = GlfwInputs
  { glfwKey :: E (Key, Int, KeyState, ModifierKeys)
  , glfwChar :: E Char
  , glfwCursorPos :: X (Double,Double)
  , glfwMouseButton :: E (MouseButton, MouseButtonState, ModifierKeys)
  , glfwScroll :: E (Double,Double)
  , glfwJoysticks :: [GlfwJoystickInput] -- ^ there are 16 joysticks
  , glfwFramebufferResize :: E (Int,Int)
  , glfwClose :: E ()
  }

data GlfwJoystickInput = GlfwJoystickInput
  { glfwJoystickPresent :: X (Maybe String) -- ^ name of joystick
  , glfwJoystickButtons :: X [JoystickButtonState]
  , glfwJoystickAxes :: X [Double]
  }

nullJoystick :: GlfwJoystickInput
nullJoystick = GlfwJoystickInput (pure Nothing) (pure []) (pure [])

errorCb :: Error -> String -> IO ()
errorCb err msg = do
  hPutStrLn stderr (show err ++ "(" ++ msg ++ ")")
  exitFailure

runGlfw :: Int -- ^ window width
        -> Int -- ^ window height
        -> String -- ^ window title
        -> IO ()  -- ^ initialization procedure
        -> (a -> IO ()) -- ^ render procedure
        -> (GlfwInputs -> E Boot -> X Time -> Setup (X a, E ())) -- ^ connect GLFW inputs to a picture signal that will be rendered on vsync
        -> IO () -- ^ returns if the @E ()@ occurs, such as window close
runGlfw winW winH title initProc renderProc setup = do
  setErrorCallback (Just errorCb)
  status <- Graphics.UI.GLFW.init
  print status
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
      joyPresentMV <- replicateM 16 newEmptyMVar
      joyButtonMV <- replicateM 16 newEmptyMVar
      joyAxisMV <- replicateM 16 newEmptyMVar
      setWindowSizeCallback  win (Just (sizeCb (putMVar windowMV)))
      setKeyCallback         win (Just (keyCb (putMVar keyMV)))
      setCharCallback        win (Just (charCb (putMVar charMV)))
      setMouseButtonCallback win (Just (clickCb (putMVar clickMV)))
      setCursorPosCallback   win (Just (mouseCb (putMVar mouseMV)))
      let x1 = map (\i mname -> putMVar (joyPresentMV !! i) mname) [0..15]
      let x2 = map (\i numStats -> putMVar (joyButtonMV !! i) numStats) [0..15]
      let x3 = map (\i axes -> putMVar (joyAxisMV !! i) axes) [0..15]
      killJoysticks <- joystickMonitor x1 x2 x3
      initProc
      _ <- forkIO $ do
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
          joysticks <- forM [0..15] $ \i -> do
            (present, setPresent) <- newX Nothing
            (buttons, setButtons) <- newX []
            (axes, setAxes) <- newX []
            input (forever (takeMVar (joyPresentMV !! i) >>= setPresent))
            input (forever (takeMVar (joyButtonMV !! i) >>= setButtons))
            input (forever (takeMVar (joyAxisMV !! i) >>= setAxes))
            return (GlfwJoystickInput present buttons axes)
          let glfwIns = GlfwInputs onKey onChar mouse onClick never
                  joysticks onResize onClose
          (scene, exit) <- setup glfwIns onBoot time
          output
            (\_ x -> putMVar graphicsMV (Just (renderProc x)))
            (snapshot_ onVsync scene)
          return exit
        killJoysticks
        putMVar graphicsMV Nothing
      forever $ do
        pollEvents
        putMVar vsyncMV ()
        mglAction <- takeMVar graphicsMV
        case mglAction of
          Nothing -> do
            terminate
            exitSuccess
          Just glAction -> glAction
        swapBuffers win
        flag <- readIORef windowClosingRef
        shouldClose <- windowShouldClose win
        when (shouldClose && flag == False) $ do
          putMVar closeMV ()
          writeIORef windowClosingRef True
      
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



joystickMonitor :: [Maybe String -> IO ()]
                -> [[JoystickButtonState] -> IO ()]
                -> [[Double] -> IO ()]
                -> IO (IO ())
joystickMonitor cb0 cb1 cb2 = do
  pollers <- replicateM 16 newEmptyMVar
  myTid <- forkIO . forever $ do
    forM_ [0..15] $ \i -> do
      b <- joystickPresent (toEnum i)
      when b $ do
        tid <- forkIO (joystickPoller (toEnum i) (cb1 !! i) (cb2 !! i))
        mprev <- tryTakeMVar (pollers !! i)
        case mprev of
          Nothing -> putMVar (pollers !! i) tid
          Just prev -> do
            killThread prev
            putMVar (pollers !! i) tid
        mname <- getJoystickName (toEnum i)
        (cb0 !! i) (Just (fromMaybe "" mname))
      when (not b) $ do
        mprev <- tryTakeMVar (pollers !! i)
        case mprev of
          Nothing -> return ()
          Just prev -> killThread prev
        (cb0 !! i) Nothing
    threadDelay 2000000
  return $ do
    killThread myTid
    forM_ pollers $ \mv -> do
      mtid <- tryTakeMVar mv
      case mtid of
        Just tid -> killThread tid
        Nothing -> return ()

-- poll joystick axes and buttons at "high enough speed"
joystickPoller :: Joystick
               -> ([JoystickButtonState] -> IO ())
               -> ([Double] -> IO ())
               -> IO ()
joystickPoller jnum buttonChange axisChange = do
  forever $ do
    mbts <- getJoystickButtons jnum
    case mbts of
      Nothing -> return ()
      --Just bts -> buttonChange bts
      Just bts -> return ()
    maxes <- getJoystickAxes jnum
    case maxes of
      Nothing -> return ()
      --Just axes -> axisChange axes
      Just axes -> return ()
    threadDelay 20000
