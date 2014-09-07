module Oscillator where

import Time
import Types
import Effect
import Data.Fixed
import qualified Data.List.Stream as S

-- Oscillator
-- Takes a waveform and a frequecy stream
-- And returns a sample stream
-- The waveform MUST have a 2 PI period
oscillator :: Waveform -> Time -> [Frequency] -> [Sample]
oscillator _ _ [] = []
oscillator w t fs = S.map snd $ S.scanl foldFunc (t, 0) fs --s : oscillator w tn fs
    where
        -- Advances time by one sample based on frequency
        timeStep :: Frequency -> Time -> Time
        timeStep f t = mod' (t + f * 2 * pi / sampleRate) (2*pi)

        foldFunc (t, _) f = (timeStep f t, w t)

--
-- Simple oscillators starting from 0 time
--
-- Should be used for synthesizing music
--

-- Simple sine oscillator
oscSin :: [Frequency] -> [Sample]
oscSin fs = oscillator sin 0 fs

-- Cosine wave
oscCos :: [Frequency] -> [Sample]
oscCos fs = oscillator cos 0 fs

-- Ramp up sawtooth:
oscSawtooth :: [Frequency] -> [Sample]
oscSawtooth fs = oscillator (\x -> -1 + x / pi) pi fs

-- Ramp down sawtooth:
oscSawtooth' :: [Frequency] -> [Sample]
oscSawtooth' fs = oscillator (\x -> 1 - x / pi) pi fs

-- Triangle, symmetric
oscTriangle :: [Frequency] -> [Sample]
oscTriangle fs = S.map ((1-) . (*2) . abs) (oscSawtooth fs)

-- Square, symmetric
oscSquare :: [Frequency] -> [Sample]
oscSquare fs = S.map (\x -> if (x) > 0 then 1 else -1) $ oscTriangle fs

--
-- LFOs
--
-- Should be used for modulation
--

-- Ramp down sawtooth, from 1 to 0
-- Mainly for LFO purposes
lfoSawtooth' :: [Frequency] -> [Sample]
lfoSawtooth' fs = oscillator (\x -> 1 - x / 2 / pi) 0 fs

-- Ramp up sawtooth, from 0 to 1
-- Mainly for LFO purposes
lfoSawtooth :: [Frequency] -> [Sample]
lfoSawtooth fs = oscillator (\x -> x / 2 / pi) 0 fs

lfoSine = (<+-> 1) . oscSin

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
eLinear s e l = eLinearPiece s e l S.++ S.repeat e

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

