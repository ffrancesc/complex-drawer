module Main where

import Complex (eval, parseExpr, showComplex)
import Control.Monad
import Data.Complex
import Data.Maybe
import Lib
import System.IO

main :: IO ()
main = forever $ do
  putStr "f(z) = " >> hFlush stdout
  l <- getLine
  let fz = maybe (const (1 / 0)) eval (parseExpr l)
  forM_ [1 .. 1000] $ \z -> putStrLn $ "f(" <> show z <> ") = " <> show (fz (z :+ 0))