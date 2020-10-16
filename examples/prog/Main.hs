#!/usr/bin/env stack
-- stack script --compile --resolver nightly-2020-09-16 --package data-ordlist --package pqueue --package vector --package deepseq

import           System.Environment (getArgs)
import           Control.DeepSeq
import           Control.Exception (evaluate)
import           Control.Monad (void, forM_)
import qualified Data.PQueue.Prio.Min as PQ
import           Data.List.Ordered (union)
import           Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector
import           Control.Monad.ST

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
run unknownArgs = putStrLn $ "Unknown args " ++ show unknownArgs 

profile :: NFData a => (t -> a) -> t -> IO ()
profile f a = void $ evaluate $ force $ {-# SCC "program" #-} f a

countSemiprimes :: Int -> [(Int, Int)] -> [Int]
countSemiprimes
  n = map (\(p, q) -> noOfSemiprimes ! (q + 1) - noOfSemiprimes ! p)
  where
    noOfSemiprimes =
      Vector.scanl (\no isSemiprime -> no + fromEnum isSemiprime) 0
      $ isNSemiprime n

isNSemiprime :: Int -> Vector Bool
isNSemiprime limit = runST
  $ do
    a <- MVector.new (limit + 1)
    let nSemiprimes = semiprimesTo limit
    forM_ nSemiprimes (\i -> MVector.unsafeWrite a i True)
    Vector.unsafeFreeze a

semiprimesTo :: Integral a => a -> [a]
semiprimesTo limit = takeWhile (<= limit) semiprimes

semiprimes :: Integral a => [a]
semiprimes = foldr (\p r -> 2 * p:union (tail $ facts p) r) [] primes
  where
    facts prime = [prime * p | p <- primesTo prime]

primesTo :: Integral a => a -> [a]
primesTo limit = takeWhile (<= limit) primes

{-- The code for the sieve with the wheel is adapted from "The Genuine Sieve of Eratosthenes" Melissa E. O’Neill -}
sieve :: Integral a => [a] -> [a]
sieve [] = []
sieve (p:candidates) = p:sieve' candidates (crossoff p candidates PQ.empty)
  where
    crossoff prime as = PQ.insert (prime * prime) (map (* prime) as)

    nextComposite = fst . PQ.findMin

    reinsert facts = PQ.insert (head facts) (tail facts)

    sieve' [] _ = []
    sieve' (c:cs) table
      | c < nextComposite table = c:sieve' cs (crossoff c cs table)
      | otherwise = sieve' cs $ adjust table c

    adjust table till
      | n <= till = adjust (reinsert facts table') till
      | otherwise = table
      where
        ((n, facts), table') = PQ.deleteFindMin table

primes :: Integral a => [a]
primes = small ++ large
  where
    small = [2, 3, 5, 7]

    large = sieve $ spin wheel2357 11

    wheel2357 = cycle
      [ 2
      , 4
      , 2
      , 4
      , 6
      , 2
      , 6
      , 4
      , 2
      , 4
      , 6
      , 6
      , 2
      , 6
      , 4
      , 2
      , 6
      , 4
      , 6
      , 8
      , 4
      , 2
      , 4
      , 2
      , 4
      , 8
      , 6
      , 4
      , 6
      , 2
      , 4
      , 6
      , 2
      , 6
      , 6
      , 4
      , 2
      , 4
      , 6
      , 2
      , 6
      , 4
      , 2
      , 4
      , 2
      , 10
      , 2
      , 10]

    spin wheel n = n:spin (tail wheel) (n + head wheel)

