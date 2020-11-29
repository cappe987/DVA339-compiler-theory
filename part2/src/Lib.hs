module Lib
    ( someFunc
    ) where

import Lex
import HappyParser
import PrettyPrinter
import Interpreter

someFunc :: IO ()
someFunc = test23


test21 :: IO ()
test21 = do 
  orig <- getContents
  case happyParser $ alexScanTokens orig of 
    Error _ -> putStrLn "false"
    Ok    _ -> putStrLn "true"

test22 :: IO ()
test22 = do
  orig <- getContents

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
      -- print $ finddiff 0 (zip cleanOriginal cleanProgram)


removeWhitespace = filter (\c -> c `notElem` [' ', '\n'])

-- Debugging function
finddiff i ((a,b):xs) = if a /= b then i else finddiff (i+1) xs
finddiff i [] = i


test23 :: IO () 
test23 = do
  let (res, output) = testing 
  putStr output
  case res of
    Right p -> print p
    Left err -> putStrLn $ "INTERPRETATION ERROR: \n" ++ err