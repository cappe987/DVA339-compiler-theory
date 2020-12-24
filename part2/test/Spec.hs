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
-- import Interpreter
import Typechecker
import Renamer
import Codegenerator

main :: IO ()
main = test_codegen


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
      expectedRes = parseExpectedOutput file

  writeFile (dir ++ "temp.txt") code
  res <- readProcess "mono" ["Trac42VM.exe", dir ++ "temp.txt"] ""
  -- putStr s
  if lines res == expectedRes then do
     putStrLn $ "Passed: " ++ s
     return True
  else do
    putStrLn $ "Failed: " ++ s
    putStrLn $ "  Expected: " ++ show (unlines expectedRes)
    putStrLn $ "  Got     : " ++ show res
    return False

test_codegen :: IO ()
test_codegen = do 
  let dir = "test_suite_25/"
  files <- sort . filter (".t42" `isSuffixOf`) <$> listDirectory dir

  res <- mapM (testCodegenFile dir) files

  putStrLn $ 
    "Passed " ++ show (length (filter (==True) res)) ++ " of " ++ show (length res)

  removeFile (dir ++ "temp.txt")

