module Lib
    ( someFunc
    ) where

import Tokens
import HappyParser
import PrettyPrinter

someFunc :: IO ()
someFunc = do
  orig <- getContents
  -- print $ happyParser $ alexScanTokens s


  let result = happyParser $ alexScanTokens orig
  case result of
    Error _ -> putStrLn "false, failed parsing"
    Ok    program -> do
      -- putStrLn "true"
      -- writeFile "prettyprint.c" (prettyPrint program)
      let cleanProgram = removeWhitespace (prettyPrint program)
          cleanOriginal = removeWhitespace orig

      if cleanOriginal == cleanProgram then 
        putStrLn "true"
      else
        putStrLn "false, failed printing"
      -- putStrLn cleanProgram
      -- putStrLn cleanOriginal
      -- print $ "RESULT: " ++ show (cleanProgram == cleanOriginal)
      -- print $ finddiff 0 (zip cleanOriginal cleanProgram)
      -- putStrLn $ prettyPrint program

      -- print program


removeWhitespace = filter (\c -> c `notElem` [' ', '\n'])


-- Debugging function
finddiff i ((a,b):xs) = if a /= b then i else finddiff (i+1) xs
finddiff i [] = i