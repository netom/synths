module Effect where

import Time
import Types
import Data.Fixed

--
-- Some convenience operators
--

-- Scaling with constant
(<*->) :: (Fractional a) => [a] -> a -> [a]
(<*->) = flip $ (flip (zipWith (*))) . repeat
infixl 7 <*->

-- Multiplying two streams (AM)
(<**>) :: (Num a) => [a] -> [a] -> [a]
(<**>) = zipWith (*)
infixl 7 <**>

-- Adding two streams (Mixing)
(<++>) :: (Num a) => [a] -> [a] -> [a]
(<++>) = zipWith (+)
infixl 6 <++>

-- Amount, input, output
distortion :: Double -> Sample -> Sample
distortion amount i = max (-1) (min 1 (amount * i))

mixN :: [[Sample]] -> [Sample]
mixN ss = sum heads / l : mixN tails
    where
        heads = map head ss
        tails = map tail ss
        l = fromIntegral $ length ss

