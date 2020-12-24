module Main where
import System.Process
import System.Directory
import Data.List
import Data.Char
import Data.Either

import Lex
import AST
import HappyParser
import PrettyPrinter
import Interpreter
import Typechecker
import Renamer
import Codegenerator

main :: IO ()
main = test_codegen


-- Some helper functions to make the chaining easier.
parse :: String -> Either String Program
parse s = case happyParser $ alexScanTokens s of Ok p -> Right p; Error err -> Left err

typechecker :: Program -> Either String [CFunction]
typechecker s = case typecheck s of Right r -> Right r; Left err -> Left (show err)

renamer :: [CFunction] -> Either String [CFunction]
renamer = Right . rename

compiler :: [CFunction] -> Either String String
compiler = 
  Right . unlines . zipWith (\i s -> show i ++ " " ++ s) [0..] . map show . compile 

runCodegen :: String -> String
runCodegen input = 
  let code = unlines $ dropWhile (\s -> not (null s) && head s == '/') $ lines input
  in case parse code >>= typechecker >>= renamer >>= compiler of
    Left err       -> error err
    Right trac -> trac

runInterpreter :: String -> String
runInterpreter input = 
  let code = unlines $ dropWhile (\s -> not (null s) && head s == '/') $ lines input
  in case parse code of
    Left err       -> error err
    Right ast -> unlines $ map (\(x:xs) -> toLower x : xs) $ (>>= words) $ lines $ snd $ runMain ast

-- toLower because VM outputs true and false, not True and False.
parseExpectedOutput :: String -> [String]
parseExpectedOutput 
  = map (\(x:xs) -> toLower x : xs)  
  . concatMap (words . drop 2) 
  . takeWhile ("//" `isPrefixOf`) 
  . lines

testCodegenFile :: String -> String -> IO Bool
testCodegenFile dir s = do 
  file <- readFile (dir ++ s)
  let code = runCodegen file
      expectedRes = unlines $ parseExpectedOutput file
      evalRes = runInterpreter file -- Test it against the interpreter as well.

  writeFile (dir ++ "temp.txt") code
  res <- readProcess "mono" ["Trac42VM.exe", dir ++ "temp.txt"] ""

  if res == expectedRes && evalRes == expectedRes then do
     putStrLn $ "Passed: " ++ s
     return True
  else do
    putStrLn $ "Failed: " ++ s
    putStrLn $ "  Expected: " ++ show expectedRes
    putStrLn $ "  Trac    : " ++ show res
    putStrLn $ "  Eval    : " ++ show evalRes
    return False

test_codegen :: IO ()
test_codegen = do 
  let dir = "test_suite_25/"
  files <- sort . filter (".t42" `isSuffixOf`) <$> listDirectory dir

  res <- mapM (testCodegenFile dir) files

  putStrLn $ 
    "Passed " ++ show (length (filter (==True) res)) ++ " of " ++ show (length res)

  removeFile (dir ++ "temp.txt")

