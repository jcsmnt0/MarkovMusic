{-# LANGUAGE MultiParamTypeClasses #-}

module Markov ( MarkovDist
              , markovDist
              , transitionDist
              , distToGen
              , gen
              , genInf
              , gen2
              , quickGen
              , StdRand
              ) where

import MonoidMap

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (second)
import Control.Monad (liftM)
import Control.Monad.Random (Rand, RandomGen, getRandomR, evalRandIO)
import Data.Foldable (foldMap)
import Data.IntMap (IntMap, keysSet, lookupGE)
import qualified Data.IntMap as IntMap
import Data.IntSet (findMax)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Semigroup
import System.Random (StdGen)
import Data.Tuple (swap)

-- | MarkovDists are convenient for storage, since they're trivial to update (monoidally!)
-- with new data
type MarkovDist a b = Map a (Map b (Sum Int))

type MarkovGen a b = Map a (IntMap b)

{- The keys of the inner IntMaps of a MarkovGen are the cumulative sums of the occurrence
   counts of their respective elements, so e.g. if we have the following distribution:
     | A | B | C   (y-axis is "from", x-axis is "to")
   --------------
   A | 1 | 3 | 0
   B | 2 | 2 | 2
   C | 0 | 0 | 1

   then the corresponding MarkovDist would be (a Map equivalent to):
   [ (A, [(A,1), (B,3)])
   , (B, [(A,2), (B,2), (C,2)])
   , (C, [(C,1)])
   ]

   and the MarkovGen would be:
   [ (A, [(1,A), (4,B)])
   , (B, [(2,A), (4,B), (6,C)])
   , (C, [(1,C)])
   ]

   This enables a straightforward way of choosing a random value with weighted
   probabilities: given a row, just choose a random integer n between 1 and the maximum key
   in the row (inclusive) and select the element with the smallest key >= n.
-}

-- Control.Newtype might be able to make this wrapping/unwrapping transparent
-- | Build a representation of a markov distribution out of a list of (from,to) pairs
tally :: (Ord a, Ord b) => [(a,b)] -> MonoidMap a (MonoidMap b (Sum Int))
tally = foldMap wrap
  where wrap (a,b) = singletonMMap a (singletonMMap b (Sum 1))

-- | Build a MarkovDist out of a list of (from,to) pairs of elements
markovDist :: (Ord a, Ord b) => [(a,b)] -> MarkovDist a b
markovDist = Map.map getMap . getMap . tally

-- | Get a list of all adjacent pairs of list elements
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

-- | Build a MarkovDist of transitions between adjacent pairs of elements in a list
transitionDist :: Ord a => [a] -> MarkovDist a a
transitionDist = markovDist . pairs

-- using mapAccum might make this a bit cleaner
-- | Convert a MarkovDist into a MarkovGen
distToGen :: MarkovDist a b -> MarkovGen a b
distToGen = Map.map $ IntMap.fromList . map (swap . second getSum) . cumSum . Map.toList
  where cumSum xs = let (elems,counts) = unzip xs in zip elems (scanl1 (<>) counts)

type StdRand = Rand StdGen

-- | Get a random element from a list
rndElem :: RandomGen g => [r] -> Rand g r
rndElem xs = (xs !!) <$> getRandomR (0, length xs - 1)

-- | Choose a random value from a map (to bootstrap a Markov walk)
bootstrap :: (Ord a, RandomGen g) => Map a b -> Rand g b
bootstrap mkv = (mkv !) <$> rndElem (Map.keys mkv)

-- | Given a MarkovGen and a starting element, generate the next element
gen :: (Ord a, RandomGen g) => MarkovGen a b -> a -> Rand g b
gen mkv prev = do
  -- fall back to random row if there's no way to proceed
  row <- maybe (bootstrap mkv) return $ Map.lookup prev mkv
  next <- getRandomR (1, findMax $ keysSet row)
  maybe (error "something blew up") (return . snd) $ lookupGE next row

-- TODO: check if this is more efficient - I suspect it's not, but it's worth investigating
-- -- | Given a MarkovDist and a starting element, randomly generate the next element in the chain
-- gen' :: Ord a => MarkovDist a b -> a -> StdRand b
-- gen' mkv prev = do
--   row <- maybe (bootstrap mkv) return $ Map.lookup prev mkv
--   let max = getSum . snd . maximumBy (compare `on` snd) . Map.toList $ row
--       row' = map (second $ (% max) . getSum) (Map.toList row)
--   Rand.fromList row'

-- | Given a MarkovGen and a random starting element, randomly generate the next element
-- in the chain
genM :: (Ord a, RandomGen g) => MarkovGen a b -> Rand g a -> Rand g b
genM mkv prev = prev >>= gen mkv

-- | Given a MarkovGen, generate an infinite Markov chain
genInf :: (Ord a, RandomGen g) => MarkovGen a a -> Rand g [a]
genInf mkv = sequence $ iterate (genM mkv) (rndElem $ Map.keys mkv)

-- | Given a MarkovGen and a length, generate a finite Markov chain
genN n = liftM (take n) . genInf

-- TODO TODO TODO TODO
-- Composing Rand values seems to be causing memory leakage, so they're being evalRandIO'ed
-- into IO values instead for now - but that's just addressing the symptom
-- it would be cool if this was polyvariadic - is that possible?
-- http://okmij.org/ftp/Haskell/vararg-fn.lhs
gen2 :: (Ord f1, Ord f2) => (a -> [f1], a -> [f2]) -> (f1 -> f2 -> b) -> a -> IO [b]
gen2 (f,g) combine x = zipWith combine
                   <$> evalRandIO (quickGen $ f x)
                   <*> evalRandIO (quickGen $ g x)

-- | Return a random Markov walk given an ordered list of elements
quickGen :: (Ord a, RandomGen g) => [a] -> Rand g [a]
quickGen = genInf . distToGen . transitionDist
