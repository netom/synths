module Time where

import Types

-- Sample rate
sampleRate :: Double
sampleRate = 44100

timeStream :: [Time]
timeStream = [0, 2*pi/sampleRate..]
