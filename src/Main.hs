

module Main where

import System.Random

import Term
import Algorithm

import Control.Monad.State

pts :: [(Double, Double)]
pts = map (\x -> (x, g x)) [-3 .. 3]
  where g x = 3*x^2 - 2*x + 7

k :: Int
k = 10


main :: IO ()
main = do
  genD <- newStdGen
  genI <- newStdGen
  let terms = take k $ evalState initializeTerms (randoms genD)
      ts = evalState (doit k pts terms) (randoms genI)
  print (head ts)
  print "-----------------------"
  print (head (tail ts))

