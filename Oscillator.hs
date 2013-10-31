module Oscillator where

import Time
import Types
import Data.Fixed

-- All oscillators must and do have an integral of 0

-- Simple sine oscillator
oscSine :: Frequency -> Time -> Sample
oscSine freq t = sin $ 2 * pi * t * freq

-- Cosine wave
oscCosine :: Frequency -> Time -> Sample
oscCosine freq t = cos $ 2 * pi * t * freq

-- FM-able sine oscillator
oscSineFm :: [Double] -> [Double] -> [Double]
oscSineFm freq t = map (\x -> sin (2*pi*x)) $ dedelta (phasedelta freq t)

-- Ramp up sawtooth:
oscSawtooth :: Frequency -> Time -> Sample
oscSawtooth freq t = 1 - 2 * mod' (freq * t) 1

-- Ramp down sawtooth:
oscSawtooth' :: Frequency -> Time -> Sample
oscSawtooth' freq t = 1 - 2 * mod' (freq * (-t)) 1

-- Triangle, symmetric
oscTriangle :: Frequency -> Time -> Sample
oscTriangle freq t = 1 - (abs (oscSawtooth freq t)) * 2

-- Square, symmetric
oscSquare :: Frequency -> Time -> Sample
oscSquare freq t = if (oscSawtooth freq t) > 0 then 1 else -1

-- Noise

noiseWhite :: [Double]
noiseWhite = map (\x -> fromIntegral x / 2^31-0.5) (noiseWhite' 0)

noiseWhite' :: Int -> [Int]
noiseWhite' seed =  a:noiseWhite' a
    where
        a = (seed * 2147483629 + 2147483587) `mod` (2^31-1)

-- modulation

am :: [Double] -> [Double] -> [Double]
am = zipWith (*)

