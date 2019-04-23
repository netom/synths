module Chord where

import Types
import Effect

type Chord = [Double]


tAug = [0, 4, 8] :: Chord
tMaj = [0, 4, 7] :: Chord
tMin = [0, 3, 7] :: Chord
tDim = [0, 3, 6] :: Chord

sDim    = [0, 3, 6,  9] :: Chord
sHDim   = [0, 3, 6, 10] :: Chord
sMin    = [0, 3, 7, 10] :: Chord
sMinMaj = [0, 3, 7, 11] :: Chord
sAug    = [0, 4, 8, 10] :: Chord
sDom    = [0, 4, 7, 10] :: Chord
sMaj    = [0, 4, 7, 11] :: Chord
sAugMaj = [0, 4, 8, 11] :: Chord

chord :: Instrument -> [Frequency] -> Chord -> [Sample]
chord i fs c = mixN $ map i $ map (\x -> map (*2**(x/12)) fs) c

doChord :: Note -> [Double] -> [Frequency]
doChord note displacement = map (\x -> (freq note)*2**(x/12)) displacement
