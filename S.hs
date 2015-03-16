module S where

import Control.Concurrent

data S a b = S (MVar a) (MVar b) (MVar ())

newS = do
  outbox <- newEmptyMVar
  inbox <- newEmptyMVar
  lock <- newMVar ()
  return (S outbox inbox lock)

callS :: S a b -> a -> IO b
callS (S outbox inbox lock) x = withMVar lock $ \_ -> do
  putMVar outbox x
  takeMVar inbox

waitS :: S a b -> (a -> IO b) -> IO ()
waitS (S outbox inbox lock) action = do
  x <- takeMVar outbox
  y <- action x
  putMVar inbox y
  return ()
