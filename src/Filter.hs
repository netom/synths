module Filter where

import Time
import Types
import Effect
import Data.Fixed
import Data.Complex
import Data.List
import qualified Data.List.Stream as S

-- Cooley-Tukey
fft :: [Complex Double] -> [Complex Double]
fft [] = []
fft [x] = [x]
fft xs = S.zipWith (+) ys ts ++ S.zipWith (-) ys ts
    where n = S.length xs
          ys = fft evens
          zs = fft odds 
          (evens, odds) = split xs
          split [] = ([], [])
          split [x] = ([x], [])
          split (x:y:xs) = (x:xt, y:yt) where (xt, yt) = split xs
          ts = S.zipWith (\z k -> exp' k n * z) zs [0..]
          exp' k n = cis $ -2 * pi * (fromIntegral k) / (fromIntegral n)

ifft :: [Complex Double] -> [Complex Double]
ifft xs = S.map conjugate $ fft $ S.map conjugate xs <*-> ((1 / fromIntegral (S.length xs)) :+ 0)

fDesign :: [Double] -> [Double]
fDesign response = blackman 0.16 (length rawTaps) <**> rawTaps
    where
        rawTaps = reverse halfiresp ++ tail halfiresp
        halfn = S.length response `div` 2
        ifftresp = S.map realPart $ ifft $ cresponse ++ reverse cresponse
        halfiresp = S.take halfn ifftresp 
        cresponse = S.map (:+ 0) response

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
-- The second parameter is the filter delay values.
-- The third parameter is the input.
fir :: [Double] -> [Double] -> [Double] -> [Double]
fir b z xs = S.map fst $ S.scanl foldFunc (0, z) xs
    where
        foldFunc (_, z) x = (S.sum $ xz <**> b, S.init xz)
            where
                xz = x : z
-- 0 < fc < 1
lp303' :: (Double, Double, Double, Double, Double) -> [Double] -> [Double] -> [Double] -> [Double] -> [Double]
lp303' (z0, z1, z2, z3, z4) (hpf:hpfs) (q:qs) (fc:fcs) (x:xs) = y : lp303' (zn0, zn1, zn2, zn3, zn4) hpfs qs fcs xs
    where
        (y, (zn0, zn1, zn2, zn3, zn4)) = lp303 (z0, z1, z2, z3, z4) hpf q fc x

lp303 :: (Double, Double, Double, Double, Double) -> Double -> Double -> Double -> Double -> (Double, (Double, Double, Double, Double, Double))
lp303 (z0, z1, z2, z3, z4) hpf q fc x = (na * y4, (zn0, zn1, zn2, zn3, zn4))
    where
        k = 20 * q;
        na = 1 + 0.5 * k;
        fc2 = fc**2

        -- hpf, nk: nagy k
        nk = fc2 * pi;
        ah = (nk - 2) / (nk + 2);
        bh = 2 / (nk + 2);

        -- run
        a = 2 * tan (0.5 * pi * fc2) -- dewarping, not required with 2x oversampling
        ainv = 1 / a
        a2 = a * a
        b  = 2 * a + 1
        b2 = b * b
        c  = 1 / (2 * a2 * a2 - 4 * a2 * b2 + b2 * b2)
        g0 = 2 * a2 * a2 * c
        g  = g0 * bh

        -- current state
        s0 = (a2 * a * z0 + a2 * b * z1 + z2 * (b2 - 2 * a2) * a + z3 * (b2 - 3 * a2) * b) * c
        s  = bh * s0 - z4

        -- input clipping
        clip x = x / (1 + abs x)
        y0 = clip $ x - k * (g * x + s) / (1 + g * k)
        y5 = g * y0 + s

        -- compute integrator outputs
        y4 = g0 * y0 + s0
        y3 = (b * y4 - z3) * ainv
        y2 = (b * y3 - a * y4 - z2) * ainv
        y1 = (b * y2 - a * y3 - z1) * ainv

        -- update filter state
        zn0 = z0 + 4*a*(y0 - y1 + y2)
        zn1 = z1 + 2*a*(y1 - 2*y2 + y3)
        zn2 = z2 + 2*a*(y2 - 2*y3 + y4)
        zn3 = z3 + 2*a*(y3 - 2*y4)
        zn4 = bh * y4 + ah * y5
