module Lib
    ( someFunc
    ) where

import Lex
import AST
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
  input <- getContents 
  
  -- Drop "expected output" comments. Comments were not in the language specification
  let code = unlines $ dropWhile (\s -> not (null s) && head s == '/') $ lines input

  let (res, output) = 
        case happyParser $ alexScanTokens code of 
          Ok program -> runMain program
          Error err  -> error err

  putStr output -- Print the accumulated output
  case res of
    Right _ -> return () 
    Left err -> putStrLn $ "INTERPRETATION ERROR: \n" ++ err


test23_file :: IO () 
test23_file = do
  input <- readFile "src/test.c"
  
  -- let (res, output) = testing 
  let program = case happyParser $ alexScanTokens input of Ok a -> a; Error err -> error err
      (res, output) = runMain program

  -- print (program :: [Function])
  putStr output
  case res of
    Right p -> return () -- print p
    Left err -> putStrLn $ "INTERPRETATION ERROR: \n" ++ err