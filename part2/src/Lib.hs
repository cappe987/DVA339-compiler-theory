module Lib
    ( someFunc
    ) where

import Lex
import AST
import HappyParser
import PrettyPrinter
import Interpreter
import Typechecker

someFunc :: IO ()
someFunc = test24

parse :: String -> E Program
parse = happyParser . alexScanTokens

test21 :: IO ()
test21 = do 
  orig <- getContents
  case parse orig of 
    Error _ -> putStrLn "false"
    Ok    _ -> putStrLn "true"

test22 :: IO ()
test22 = do
  orig <- getContents

  case parse orig of
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
        case parse code of 
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
  let program = case parse input of Ok a -> a; Error err -> error err
      (res, output) = runMain program

  -- print (program :: [Function])
  putStr output
  case res of
    Right p -> return () -- print p
    Left err -> putStrLn $ "INTERPRETATION ERROR: \n" ++ err


test24 :: IO ()
test24 = do 
  input <- getContents
  
  let code = unlines $ dropWhile (\s -> not (null s) && head s == '/') $ lines input
  -- let (res, output) = testing 
  let program = case parse code of Ok a -> a; Error err -> error err

  -- print program
  case typecheck program of 
    Left (line, col) -> putStrLn $ "fail " ++ show (line + 1) ++ " " ++ show col
    Right _ -> putStrLn "pass"

test24_file :: IO ()
test24_file = do
  input <- readFile "src/test.c"
  
  let code = unlines $ dropWhile (\s -> not (null s) && head s == '/') $ lines input
  -- let (res, output) = testing 
  let program = case parse code of Ok a -> a; Error err -> error err

  -- print program
  case typecheck program of 
    Left (line, col) -> putStrLn $ "fail " ++ show (line + 1) ++ " " ++ show col
    Right p -> putStrLn "pass" >> print p