module Lib
    ( someFunc
    ) where

import Tokens
import HappyParser

someFunc :: IO ()
someFunc = do
  s <- getContents
  print $ happyParser $ alexScanTokens s