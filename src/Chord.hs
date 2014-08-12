module Chord where

import Types
import Effect

type Chord = [Double]


tAug = [0, 4, 8] :: [Double]
tMaj = [0, 4, 7] :: [Double]
tMin = [0, 3, 7] :: [Double]
tDim = [0, 3, 6] :: [Double]

sDim    = [0, 3, 6,  9] :: [Double]
sHDim   = [0, 3, 6, 10] :: [Double]
sMin    = [0, 3, 7, 10] :: [Double]
sMinMaj = [0, 3, 7, 11] :: [Double]
sAug    = [0, 4, 8, 10] :: [Double]
sDom    = [0, 4, 7, 10] :: [Double]
sMaj    = [0, 4, 7, 11] :: [Double]
sAugMaj = [0, 4, 8, 11] :: [Double]

chord :: Instrument -> [Frequency] -> Chord -> [Sample]
chord i fs c = mixN $ map i $ map (\x -> map (*2**(x/12)) fs) c

doChord :: Note -> [Double] -> [Frequency]
doChord note displacement = map (\x -> (freq note)*2**(x/12)) displacement
