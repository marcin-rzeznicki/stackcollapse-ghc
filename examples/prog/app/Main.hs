module Main where

import           Example
import           System.Environment (getArgs)
import           Control.DeepSeq
import           Control.Exception (evaluate)
import           Control.Monad (void)

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run ["semiprimes"] = profile semiprimesTo (50000 :: Int)
run ["countSemiprimes"] = profile
  (countSemiprimes 50000)
  [ (1, 26)
  , (4, 10)
  , (16, 20)
  , (1, 100)
  , (4, 100)
  , (50, 100)
  , (90, 100)
  , (200, 10000)
  , (300, 10000)]

profile :: NFData a => (t -> a) -> t -> IO ()
profile f a = void $ evaluate $ force $ {-# SCC "program" #-} f a
