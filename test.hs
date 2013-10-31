import Time
import Types
import Effect
import Output
import Oscillator

import Control.Arrow

wobblyTime = map (\t -> t + (pmi (freq C2) 0.2) * oscSquare 1 t) time

-- music = map double2Float $ am (map (oscTriangle 440) time) (map (oscTriangle 1.02) time)
-- music = am noiseWhite  (map ((+1) . oscSawtooth 6) time)
-- music = map (\t -> (oscSawtooth (freq C2) t + oscSawtooth (freq C2 + 1.7) t)/2) time

-- Reese bass: frequency, index, time
--reese f modf i t = (oscSawtooth f t + oscSawtooth f (t + (pmi f i) * oscSine modf t))/2

--reese f modf i = 
--    oscSawtooth f
--  *** id >>> ( (id &&& id) >>> uncurry ((+) ((oscSine modf)) &&& (pmi f i)) >>> uncurry (*) >>> oscSawtooth f )   
--  >>>
--    uncurry (+) >>> (/2)

--music = map (reese (freq C2) 3 0.1) time
music = map (\t -> (1 + (distortion 8 (oscSine 12 t))) / 2 * oscSine 2300 t) time

--main = pulseaudioOutput music
main = waveOutput "test.wav" $ take (44100*30) music
