module Main (main) where

import qualified Parser as Parser
import Interpreter

import qualified Text.Parsec as Parsec
import System.Environment
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    args <- getArgs
    contents <- case args of
        [filename] -> readFile filename
        [] -> getContents
        _  -> error "Too many arguments"

    putStrLn $ case Parsec.runParser Parser.program () "" contents of
        Left err -> show err
        Right parsed -> show $ run parsed
