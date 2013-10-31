module Effect where

import Time
import Types
import Data.Fixed

-- Amount, input, output
distortion :: Double -> Sample -> Sample
distortion amount i = max (-1) (min 1 (amount * i))
