module Filter where

import Time
import Types
import Data.Fixed

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
fir b z [] = []
fir b z (x:xs) = y : fir b znext xs
    where
        dprod a b = sum $ map (uncurry (*)) (zip a b)
        znext = x : init z
        y = dprod (x : z) b
