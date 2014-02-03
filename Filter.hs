module Filter where

import Time
import Types
import Data.Fixed

epsilon :: RealFloat a => a
epsilon = encodeFloat 1 (fromIntegral $ 1-floatDigits epsilon)
 
-- Boosted from Boost http://www.boost.org/boost/math/special_functions/sinc.hpp
sinc :: (RealFloat a) => a -> a
sinc x = if abs x >= taylor_n_bound then sin x / x else 1 - x^2/6 + x^4/120
    where
        taylor_n_bound = sqrt $ sqrt epsilon

-- Construct initial conditions for lfilter.
-- Given a linear filter b and initial conditions on the output y
-- and the input x, return the inital conditions on the state vector zi
-- which is used by lfilter to generate the output given the input.
-- http://en.wikipedia.org/wiki/Digital_filter#Direct_Form_II
lfiltic :: [Double] -> [Double] -> [Double] -> [Double] -> [Double]
lfiltic b a x y = take zn $ repeat 0 -- TODO: actually compute this
    where
        an = length a
        bn = length b
        taps = max an bn
        zn = taps - 1

-- Filter a data sequence, x, using a digital filter. 
-- The first parameter is the numerator coefficient vector.
-- The second parameter os the denominator coefficient vector.
-- The third parameter is the filter delay values.
-- The fourth parameter is the input.
-- http://en.wikipedia.org/wiki/Digital_filter#Direct_Form_II
lfilter :: [Double] -> [Double] -> [Double] -> [Double] -> [Double]
lfilter b a z x = [0]
