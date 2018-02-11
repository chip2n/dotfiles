{-# LANGUAGE OverloadedStrings #-}

import DBus
import DBus.Client

main = do
  client <- connectSession
  addMatch client (matchAny { matchPath = Just "/" }) $ \signal -> putStrLn (show signal)
