module Main where

import Markov
import MonoidMap
import Piece

import Codec.Midi
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Random
import Data.Function (on)
import Data.List hiding (insert)
import Data.Map (Map(..), insert, (!))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Foldable (Foldable, foldMap)
import qualified Data.Foldable as F
import Data.Ratio

import qualified Euterpea as E
import Euterpea hiding (Note, Rest, pitch)

import Music (play')

main = do
  putStrLn "File path:"
  path <- getLine

  putStrLn "Output length (measures):"
  len <- read <$> getLine

  (m,_,_) <- readMidi path
  let piece = toPiece m

  piece' <- sequenceMap $ Map.map genPart piece
  play' $ fromPiece $ Map.map (truncatePart (len % 1) . stripBeats) piece'

-- ignoring Rests for now
beat :: Note d p -> Maybe d
beat (Note d _) = Just d
beat (Rest _) = Nothing

pitch :: Note d p -> Maybe p
pitch (Note _ p) = Just p
pitch (Rest _) = Nothing

beats = catMaybes . map beat
pitches = catMaybes . map pitch

-- TODO TODO TODO TODO
-- Composing Rand values seems to be causing memory leakage, so they're being evalRandIO'ed
-- into IO values instead for now - but that's just addressing the symptom
genPart :: (Ord d, Ord p) => Part (Chord d p) -> IO (Part (Chord d p))
genPart = gen2 (beats, pitches) Note

readMidi :: FilePath -> IO (Music1, Context (Pitch, [NoteAttribute]), UserPatchMap)
readMidi path = importFile path >>= either error (return . fromMidi)

-- sequence for Maps
sequenceMap :: Ord k => Map k (IO v) -> IO (Map k v)
sequenceMap = Map.foldrWithKey f (return Map.empty)
  where f k v acc = do
          acc' <- acc
          v' <- v
          return $ Map.insert k v' acc'
