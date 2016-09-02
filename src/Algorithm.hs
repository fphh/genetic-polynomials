

module Algorithm (initializeTerms, doit) where

import qualified Data.List as List
import Data.Ord (comparing, Down(..))

import System.Random

import Control.Monad
import Control.Monad.State
import Control.Monad.Extra

import Control.Applicative

import Term



type Pt = (Double, Double)


getRandom :: State [a] a
getRandom = do
  (x:xs) <- get
  put xs
  return x

initializeTerms :: State [Double] [Term]
initializeTerms = do
  s <- liftA2 linear getRandom getRandom
  t <- liftA3 quadratic getRandom getRandom getRandom
  fmap ((s:) . (t:)) initializeTerms

termWithHole :: [(Term, Term)] -> State [Int] (Term, Term)
termWithHole ts = liftA ((ts !!) . (`mod` length ts)) getRandom

termsWithHoles :: [Term] ->  State [Int] [(Term, Term)]
termsWithHoles = mapM termWithHole . map holes

combine :: (Term, Term) -> [(Term, Term)] -> [Term]
combine t = List.foldr f [] . map (haveSex t)
  where f (a, b) acc = a:b:acc

goodnessOfFit :: ([Double], [Double]) -> Term -> (Down Double, Term)
goodnessOfFit (xs, ys) t = (Down $ sum $ zipWith f xs ys, t)
  where f x y = let a = eval x t in a*a

reduceTo :: Int -> ([Double], [Double]) -> [Term] -> [Term]
reduceTo k ps ts =
  take k $ map snd $ List.sortBy (comparing fst) $ map (goodnessOfFit ps) ts

everyoneWithEveryone ::
  Int -> ([Double], [Double]) -> [Term] -> State [Int] [Term]
everyoneWithEveryone q ps =
  let go [] = []
      go (y:ys) = combine y ys ++ go ys
      reduce = reduceTo q ps 
  in liftA (reduce . go) . termsWithHoles

doit :: Int -> [(Double, Double)] -> [Term] -> State [Int] [[Term]]
doit k pts = iterateM (everyoneWithEveryone k (unzip pts))
