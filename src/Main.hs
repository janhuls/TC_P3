module Main where

import CSharp.Algebra
import CSharp.Analysis
import CSharp.CodeGen
import CSharp.AbstractSyntax
import CSharp.Parser
import SSM
import ParseLib.Derived
import System.Environment
import System.FilePath
import System.Exit
import System.IO

main :: IO ()
main = do
  args <- getArgs
  files <- case args of
    [] -> do
      putStrLn "no argument given; assuming example.cs"
      return ["example.cs"]
    xs -> return xs
  mapM_ processFile files

processFile :: FilePath -> IO ()
processFile infile = do
  let outfile = addExtension (dropExtension infile) "ssm"
  xs <- readFile infile
  
  -- parse prog
  let program = run "parser" (pClass <* eof) . run "lexer" lexicalScanner $ xs
  
  -- analyze prog (--sidenote voor jan: ik weet niet of dit uberhaupt nodig is voor de autograder, vgm niet dit moet je even checken, dit hangt samen met de system.exit en system.io imports hier en in analysis.hs, dit moet je checken voor de autograder van morgen)
  analyzeClass program
  
  -- gen code
  let ssm = formatCode $ foldCSharp codeAlgebra program
  writeFile outfile ssm
  putStrLn (outfile ++ " written")

run :: (Ord s, Show a) => String -> Parser s a -> [s] -> a
run s p x = fst . headOrError . parse (p <* eof) $ x
  where
    headOrError (x : xs) = x
    headOrError [] = error $ "The " <> s <> " returned no full parses."