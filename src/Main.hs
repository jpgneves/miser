module Main where

import System.Environment
import Text.Miser.Thrift.Parser

main :: IO ()
main = do
  (file:_) <- getArgs
  result <- parseFile file
  case result of
    Left e   -> print e
    Right xs -> print xs
