{-# Language BangPatterns #-}
module Filter where

import Time
import Types
import Effect
import Data.Fixed
import Data.Complex
import Data.List

-- Cooley-Tukey
fft :: [Complex Double] -> [Complex Double]
fft [] = []
fft [x] = [x]
fft xs = zipWith (+) ys ts ++ zipWith (-) ys ts
    where n = length xs
          ys = fft evens
          zs = fft odds 
          (evens, odds) = split xs
          split [] = ([], [])
          split [x] = ([x], [])
          split (x:y:xs) = (x:xt, y:yt) where (xt, yt) = split xs
          ts = zipWith (\z k -> exp' k n * z) zs [0..]
          exp' k n = cis $ -2 * pi * (fromIntegral k) / (fromIntegral n)

ifft :: [Complex Double] -> [Complex Double]
ifft xs = map conjugate $ fft $ map conjugate xs <*-> ((1 / fromIntegral (length xs)) :+ 0)

fDesign :: [Double] -> [Double]
fDesign response = blackman 0.16 (length rawTaps) <**> rawTaps
    where
        rawTaps = reverse halfiresp ++ tail halfiresp
        halfn = length response `div` 2
        ifftresp = map realPart $ ifft $ cresponse ++ reverse cresponse
        halfiresp = take halfn ifftresp 
        cresponse = map (:+ 0) response

blackman :: Double -> Int -> [Double]
blackman a n = [a0 - a1 * cos (2*pi*j/(fromIntegral n - 1)) + a2 * cos (4*pi*j/(fromIntegral n - 1))  | i <- [1..n], let j = fromIntegral i ]
    where
        a0 = (1 - a) / 2
        a1 = 1 / 2
        a2 = a / 2

-- The smallest number >0 that is expressible with a Double
epsilon :: Double
epsilon = encodeFloat 1 (fromIntegral $ 1-floatDigits epsilon)
 
-- Boosted from Boost http://www.boost.org/boost/math/special_functions/sinc.hpp
sinc :: Double -> Double
sinc x = if abs x >= taylor_n_bound then sin x / x else 1 - x^2/6 + x^4/120
    where
        taylor_n_bound = sqrt $ sqrt epsilon

-- Filter a data sequence, x, using a digital filter. 
-- The first parameter is the coefficient vector.
-- The third parameter is the filter delay values.
-- The fourth parameter is the input.
fir :: [Double] -> [Double] -> [Double] -> [Double]
fir !b !z xs = map fst $ scanl core (0, z) xs
    where
        core (y, z) x = (sum ((x : z) <**> b), x : init z)
