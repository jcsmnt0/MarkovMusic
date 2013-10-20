{-# LANGUAGE PostfixOperators #-}

module Music where

import Control.Applicative
import Control.Monad
import Euterpea hiding (Mode)
import System.Cmd (rawSystem)

instance Functor Music where
  fmap f (Prim (Note dur x)) = Prim $ Note dur (f x)
  fmap _ (Prim (Rest dur)) = Prim $ Rest dur
  fmap f (x :+: y) = fmap f x :+: fmap f y
  fmap f (x :=: y) = fmap f x :=: fmap f y
  fmap f (Modify m x) = Modify m (fmap f x)

-- | Euterpea's play function doesn't seem to work with my sound card setup, so this is a
-- hackish approximation of it
play' :: Performable a => Music a -> IO ()
play' tune = test tune
          <* rawSystem "fluidsynth"
               [ "-a", "alsa"
               , "-m", "alsa_seq"
               , "-l"
               , "-i", "/usr/share/soundfonts/fluidr3/FluidR3GM.SF2"
               , "test.mid"
               ]

loop :: Int -> [a] -> [a]
loop n = concat . replicate n

-- | Just for clarity in type signatures - many of these functions use relative pitch offsets
type RelPitch = AbsPitch
relPitch = absPitch

type Music' = Music Pitch

-- | Alter a pitch by an offset
alterPitch :: RelPitch -> Pitch -> Pitch
alterPitch offset = pitch . (+ offset) . relPitch

alter :: Music' -> RelPitch -> Music'
alter m rel = fmap (alterPitch rel) m

cluster :: [RelPitch] -> Music' -> Music'
cluster ps m = chord $ map (alter m) ps

progression :: [RelPitch] -> Music' -> Music'
progression ps m = line $ map (alter m) (accumulate ps)
  where accumulate = scanl (+) 0

(.+) = flip progression
(.=) = flip cluster

rotateLeft :: [a] -> Int -> [a]
rotateLeft xs n = drop n xs ++ take n xs

data Mode = Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian
          deriving (Eq, Ord, Enum, Show)
 
data PitchSet = I | II | III | IV | V | VI | VII
              | Flat PitchSet | Sharp PitchSet
              deriving (Eq, Show)
 
-- postfix
(|.) = Flat
(#) = Sharp

instance Enum PitchSet where
  toEnum n = case n of
    1 -> I; 2 -> II; 3 -> III; 4 -> IV; 5 -> V; 6 -> VI; 7 -> VII
  fromEnum n = case n of
    I -> 1; II -> 2; III -> 3; IV -> 4; V -> 5; VI -> 6; VII -> 7

inMode :: Music (PitchSet, Octave) -> (PitchClass, Mode) -> Music'
inMode music (pit,mode) = fmap toPitch music
  where toPitch (Flat pc,oct) = alterPitch (-1) $ toPitch (pc,oct)
        toPitch (Sharp pc,oct) = alterPitch 1 $ toPitch (pc,oct)
        toPitch (pc,oct) = alterPitch (intervals mode !! fromEnum pc) (pit,oct)
        intervals m = scanl (+) 0 $ rotateLeft diatonic (fromEnum m)
        diatonic = [2,2,1,2,2,2,1]

data Interval = Root | P4 | P5 | Oct
              | Maj2 | Maj3 | Maj6 | Maj7
              | Min2 | Min3 | Min6 | Min7
              | Aug1 | Aug2 | Aug3 | Aug4 | Aug5 | Aug6 | Aug7 | Aug8 
              | Dim2 | Dim3 | Dim4 | Dim5 | Dim6 | Dim7 | Dim8 

interval :: Interval -> RelPitch
interval i = case i of
  Root -> 0; P4 -> 5; P5 -> 7; Oct -> 12
  Maj2 -> 2; Maj3 -> 4; Maj6 -> 9; Maj7 -> 11
  Min2 -> 1; Min3 -> 3; Min6 -> 8; Min7 -> 10
  Aug1 -> 1; Aug2 -> 3; Aug3 -> 5; Aug4 -> 6; Aug5 -> 8; Aug6 -> 10; Aug7 -> 12; Aug8 -> 13
  Dim2 -> 0; Dim3 -> 2; Dim4 -> 4; Dim5 -> 6; Dim6 -> 7; Dim7 -> 9; Dim8 -> 11

type Chord = [Interval]

majTriad = [Root, Maj3, P5]
minTriad = [Root, Min3, P5]
augTriad = [Root, Maj3, Aug5]
dimTriad = [Root, Min3, Dim5]
susTriad = [Root, P4, P5]

inChord :: Music' -> Chord -> Music'
inChord m c = chord $ map (alter m) (map interval c)

major = Ionian
minor = Aeolian

pitchClasses = [ Cff, Cf, C, Dff, Cs, Df, Css, D, Eff, Ds, Ef, Fff, Dss, E, Ff, Es, F, Gff
               , Ess, Fs, Gf, Fss, G, Aff, Gs, Af, Gss, A, Bff, As, Bf, Ass, B, Bs, Bss
               ]

modeScale :: Dur -> (PitchClass, Mode) -> Music'
modeScale dur mod = line [ Prim (Note dur (pc,oct)) `inMode` mod
                         | pc <- [I,II,III,IV,V,VI,VII], oct <- [0..7]
                         ]
