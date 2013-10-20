{-# LANGUAGE TupleSections, FlexibleInstances, FlexibleContexts, DeriveFunctor #-}

-- | This module defines a few types as an alternative to Euterpea's Music structure; Music
-- is more flexible but also somewhat ambiguous (e.g. depending on the values of a and b,
-- a :=: b might represent either a single chord or two independent simultaneous parts,
-- each of which might contain chords)
module Piece where

import Control.Arrow
import Data.Function
import Data.List hiding (insert)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map, insert, (!))
import qualified Data.Map as Map
import Data.Ratio
import Data.Semigroup
import Euterpea hiding (Rest, Note, note, Mode, chord)
import qualified Euterpea as E

import Music () -- just for Functor instance

data Note d p = Note d p | Rest d
              deriving (Show, Eq, Ord, Functor)

type Chord d p = Note d [p]
type Part = []
type Piece a = Map InstrumentName (Part a)

newtype Flip f a b = Flip { unflip :: f b a }

fmap' :: (Functor (Flip f c)) => (a -> b) -> f a c -> f b c
fmap' f = unflip . fmap f . Flip

sconcatMap :: Semigroup b => (a -> b) -> NonEmpty a -> b
sconcatMap f = sconcat . fmap f

instance Functor (Flip Note p) where
  fmap f (Flip (Note d p)) = Flip $ Note (f d) p
  fmap f (Flip (Rest d)) = Flip $ Rest (f d)

type Beat = Dur

-- Keeps the rightmost duration, or the rightmost Rest if there are no Notes.
-- For predictable behavior, both operands should have the same duration (or
-- irrelevant durations)
instance Semigroup p => Semigroup (Note d p) where
  Rest _ <> r@(Rest _) = r
  Rest _ <> n@(Note _ _) = n
  n@(Note _ _) <> Rest _ = n
  Note _ n1 <> Note d n2 = Note d (n1 <> n2)

-- truncate a part after a given number of beats
truncatePart :: (Num d, Ord d) => d -> Part (Note d a) -> Part (Note d a)
truncatePart cutoff = tail . map snd . takeWhile ((<= cutoff) . fst) . scanl totalDurs empty
  where empty = (fromInteger 0, error "placeholder") -- can this be avoided?
        totalDurs (d1,a) r@(Rest d2) = (d1+d2, r)
        totalDurs (d1,a) n@(Note d2 _) = (d1+d2, n)

convertNote :: Music (a,Beat) -> Note (Dur,Beat) a
convertNote (Prim (E.Rest d)) = Rest (d, 0 % 1) -- TODO - I have no beat info for rests
convertNote (Prim (E.Note d (p,b))) = Note (d,b) p

concatNotes :: Eq d => [Note d p] -> Chord d p
concatNotes = sconcatMap (fmap (:[])) . NonEmpty.fromList

-- inverse of Euterpea.chord
unchord :: Music a -> [Music a]
unchord (a :=: b) = a : unchord b
unchord (Prim (E.Rest dur)) | (dur == 0 % 1) = []
unchord _ = error "unsupported music structure"

remainder :: Integral n => Ratio n -> Ratio n
remainder n = (num `mod` den) % den
  where (num,den) = (numerator n, denominator n)

unmodify :: Music a -> Music a
unmodify (Modify _ m) = m
unmodify (a :=: b) = unmodify a :=: unmodify b
unmodify (a :+: b) = unmodify a :+: unmodify b
unmodify m = m

-- instrument assignments happen only at the outermost level of Music in fromMidi
instrumentMap :: E.Music a -> Map InstrumentName (E.Music a)
instrumentMap (Modify (Instrument inst) m1 :=: m2) = insert inst m1 $ instrumentMap m2
instrumentMap (Prim (E.Rest dur)) | dur == (0 % 1) = Map.empty
instrumentMap _ = error "unsupported music structure"

lstrip :: E.Music a -> E.Music a
lstrip (Prim (E.Rest dur) :+: m) | (dur == 0 % 1) = lstrip m
lstrip (Prim (E.Rest dur) :=: m) | (dur == 0 % 1) = lstrip m
lstrip (Modify m n) = Modify m (lstrip n)
lstrip m = m

stripAttributes :: Music1 -> Music Pitch
stripAttributes = fmap fst

-- assumes the format that Euterpea.fromMidi emits
restructure :: E.Music a -> Part [E.Music (a,Beat)]
restructure = map (map fixOffset) . groupBy ((==) `on` absOffset) . unchord
  where fixOffset (Prim (E.Rest dur) :+: m) = fmap (,remainder dur) m
        fixOffset _ = error "unsupported music structure"
        absOffset (Prim (E.Rest d) :+: a) = d
        absOffset _ = error "unsupported music structure"

-- assumes the format that Euterpea.fromMidi emits
toPiece :: Music1 -> Piece (Chord (Dur,Beat) Pitch)
toPiece = Map.map convertInstrument . instrumentMap
  where convertInstrument = map (concatNotes . map convertNote)
                          . restructure
                          . unmodify
                          . stripAttributes
                          . lstrip

fromPiece :: Piece (Chord Dur Pitch) -> Music Pitch
fromPiece = E.chord . Map.foldrWithKey addInstrument []
  where addInstrument inst part song = Modify (Instrument inst) (fromPart part) : song
        fromPart = line . map fromChord
        fromChord (Note d ps) = E.chord [Prim $ E.Note d p | p <- ps]
        fromChord (Rest d) = Prim $ E.Rest d

stripBeats :: Part (Note (d,Beat) p) -> Part (Note d p)
stripBeats = map (fmap' fst)
