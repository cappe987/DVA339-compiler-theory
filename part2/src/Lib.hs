module Lib
    ( someFunc
    ) where

import Tokens
import HappyParser

someFunc :: IO ()
someFunc = do
  s <- getContents
  -- print $ happyParser $ alexScanTokens s
  case happyParser $ alexScanTokens s of
    Ok    _ -> putStrLn "True"
    Error e -> putStrLn "False"