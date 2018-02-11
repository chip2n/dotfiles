{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar, takeMVar)
import System.Posix.Signals (Handler, Handler(CatchOnce), installHandler, sigINT, sigTERM)
import System.IO (hWaitForInput, stdin, stdout, hPutStrLn, hSetBuffering, BufferMode(..))
import Lib
import DBus
import DBus.Client
import DBus.Internal.Types

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  loopUntilInterruption setupDBus cleanUp
  return ()

cleanUp :: Client -> IO ()
cleanUp client = do
  disconnect client

setupDBus :: IO Client
setupDBus = do
  client <- connectSession
  signalHandler <- addMatch client xmonadMatch processSignal
  return client

xmonadMatch :: MatchRule
xmonadMatch = matchAny { matchPath = Just "/org/xmonad/Log" }

processSignal :: Signal -> IO ()
processSignal signal = do
  let x = head $ signalBody signal :: Variant
      y = toValue $ x
  case (fromVariant x :: Maybe String) of
    Just str -> hPutStrLn stdout str
    Nothing  -> return ()

loopUntilInterruption :: IO a -> (a -> IO ()) -> IO ()
loopUntilInterruption p finish = do
  v <- newEmptyMVar
  installHandler sigINT (handler v) Nothing
  installHandler sigTERM (handler v) Nothing
  result <- p
  loop v
  finish result
  return ()

handler :: MVar () -> Handler
handler v = CatchOnce $ putMVar v ()

loop :: MVar () -> IO ()
loop v = do
  val <- takeMVar v
  return ()
