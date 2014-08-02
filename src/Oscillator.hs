{-# Language Arrows #-}
module Oscillator where

import Time
import Types
import Effect
import Data.Fixed
import FRP.Yampa
import qualified Data.List.Stream as S

oscillator :: Waveform -> Frequency -> SF CV Sample
oscillator w f0 = proc cv -> do
    let f = f0 * (2 ** cv)
    phi <- integral -< 2 * pi * f
    returnA -< w phi

-- Simple sine oscillator
oscSin :: Frequency -> SF CV Sample
oscSin = oscillator sin

-- Cosine wave
oscCos :: Frequency -> SF CV Sample
oscCos = oscillator cos

-- Ramp up sawtooth:
oscSawtooth :: Frequency -> SF CV Sample
oscSawtooth = oscillator (\x -> -1 + x / pi)

-- Ramp down sawtooth:
oscSawtooth' :: Frequency -> SF CV Sample
oscSawtooth' = oscillator (\x -> 1 - x / pi)

-- Triangle, symmetric
oscTriangle :: Frequency -> SF CV Sample
oscTriangle = oscSawtooth >>> arr ((1-) . (*2) . abs)

-- Square, symmetric
oscSquare :: Frequency -> SF CV Sample
oscSquare fs = oscTriangle >>> arr (\x -> if (x) > 0 then 1 else -1)

--
-- LFOs
--
-- Should be used for modulation
--

-- Ramp down sawtooth, from 1 to 0
-- Mainly for LFO purposes
lfoSawtooth' :: Frequency -> SF CV Sample
lfoSawtooth' = oscillator (\x -> 1 - x / 2 / pi)

-- Ramp up sawtooth, from 0 to 1
-- Mainly for LFO purposes
lfoSawtooth :: Frequency -> SF CV Sample
lfoSawtooth = oscillator (\x -> x / 2 / pi)

--
-- Envelopes
--
-- Envelopes are infinite lists that are often rather slow-changing.
--

-- Exponential envelope
-- Parameters are start value and the decay coefficient c
eExp :: Double -> Double -> [Double]
eExp s c = s : eExp (s*c) c

-- Auxiliary linear slope function (finite)
eLinearPiece :: Double -> Double -> Time -> [Double]
eLinearPiece s e l = [s + i*(e-s)/samples | i <- [1,2..samples]]
    where
        samples = l * sampleRate

-- Linear envelope
-- Parameters: start, end, length
eLinear :: Double -> Double -> Time -> [Double]
eLinear s e l = eLinearPiece s e l ++ S.repeat e

-- ADSR envelope
eADSR :: Double -> Double -> Double -> Double -> Double -> [Sample]
eADSR a d s r len = (
    eLinearPiece 0 1 a ++
    eLinearPiece 1 s d ++
    S.take sn (S.repeat s) ++
    eLinearPiece s 0 r ++
    S.repeat 0
    )
    where
        sn = max 0 $ round $ sampleRate * len - (sampleRate * a + sampleRate * d)

--
-- Noise
--

noiseWhite :: [Double]
noiseWhite = S.map (\x -> fromIntegral x / 2^31-0.5) (noiseWhite' 0)
    where
        noiseWhite' :: Int -> [Int]
        noiseWhite' seed =  a:noiseWhite' a
            where
                a = (seed * 2147483629 + 2147483587) `mod` (2^31-1)

