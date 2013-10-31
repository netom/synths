module Time where

import Types

-- Sample rate
sampleRate :: Double
sampleRate = 44100

-- Phase modulation index in period
-- Add this to time to get PM
pmi :: Frequency -> Double -> Double
pmi f p = p/f

-- Time, represented as contignously increasing value
time :: [Double]
time = [0,(1/sampleRate)..]

-- Derivative of time, expressed explicitly
time' :: [Double]
time' = repeat (1/sampleRate)

-- Return the differences of a list
delta :: [Double] -> [Double]
delta (x:xs) = x : delta' (x:xs)
delta [] = []

delta' :: [Double] -> [Double]
delta' (x:y:xs) = y-x : delta' (y:xs)
delta' _ = []

-- Reconstruct a list from the differences
dedelta :: [Double] -> [Double]
dedelta (x:xs) = x : dedelta' (x:xs)
dedelta [] = []

dedelta' :: [Double] -> [Double]
dedelta' (x:y:xs) = y+x : dedelta' (y+x:xs)
dedelta' _ = []

-- Return phace changes from frequency and time
phasedelta :: [Double] -> [Double] -> [Double]
phasedelta freq t = map ( uncurry (*) ) $ zip freq $ delta t

--main = print 2
