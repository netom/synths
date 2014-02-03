module Effect where

import Time
import Types
import Data.Fixed

-- Amount, input, output
distortion :: Double -> Sample -> Sample
distortion amount i = max (-1) (min 1 (amount * i))

mix :: [Sample] -> [Sample] -> [Sample]
mix s1 s2 = map ((/2) . uncurry (+)) (zip s1 s2)

mixN :: [[Sample]] -> [Sample]
mixN ss = sum heads / l : mixN tails
    where
        heads = map head ss
        tails = map tail ss
        l = fromIntegral $ length ss

am :: [Sample] -> [Sample] -> [Sample]
am = zipWith (*)

--
-- Envelopes
--

-- ID envelope
eID :: Double -> [Sample] -> [Sample]
eID len samples = take (round $ sampleRate * len) samples


eADSR :: Double -> Double -> Double -> Double -> Double -> [Sample] -> [Sample]
eADSR a d s r len samples = am samples envelope
    where
        lens = sampleRate * len
        an = round $ sampleRate * a
        dn = sampleRate * d
        sn = len - (a + d)
        rn = sampleRate * r
        envelope = take an [0,1 / fromIntegral an..]
        
        
