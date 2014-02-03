module Oscillator where

import Time
import Types
import Data.Fixed

-- Oscillator
-- Takes a waveform and a frequecy stream
-- And returns a sample stream
-- The waveform MUST have a 2 PI period
oscillator :: Waveform -> Time -> [Frequency] -> [Sample]
oscillator _ _ [] = []
oscillator w t (f:fs) = s : oscillator w tn fs
    where
        -- Advances time by one sample based on frequency
        timeStep :: Frequency -> Time -> Time
        timeStep f t = mod' (t + f * 2 * pi / sampleRate) (2*pi)

        -- Oscillator core
        -- It takes a waveform, a frequency and current time
        -- and returns the sample at that point in time, and the next time.
        -- This is a useful function, so I wrote this properly in order
        -- to be easier to refactor it later for toher uses
        oscCore :: Waveform -> Frequency -> Time -> (Sample, Time)
        oscCore w f t = (w t, timeStep f t)

        (s, tn) = oscCore w f t

--
-- Simple oscillators starting from 0 time
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
oscTriangle fs = map ((1-) . (*2) . abs) (oscSawtooth fs)

-- Square, symmetric
oscSquare :: [Frequency] -> [Sample]
oscSquare fs = map (\x -> if (x) > 0 then 1 else -1) $ oscTriangle fs

--
-- Noise
--

noiseWhite :: [Double]
noiseWhite = map (\x -> fromIntegral x / 2^31-0.5) (noiseWhite' 0)
    where
        noiseWhite' :: Int -> [Int]
        noiseWhite' seed =  a:noiseWhite' a
            where
                a = (seed * 2147483629 + 2147483587) `mod` (2^31-1)

