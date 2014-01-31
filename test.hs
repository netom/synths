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
reese f modf i t = (oscSawtooth f t + oscSawtooth f (t + (pmi f i) * oscSine modf t))/2

--reese f modf i = 
--    oscSawtooth f
--  *** id >>> ( (id &&& id) >>> uncurry ((+) ((oscSine modf)) &&& (pmi f i)) >>> uncurry (*) >>> oscSawtooth f )   
--  >>>
--    uncurry (+) >>> (/2)

--music = map (reese (freq C2) 3 0.1) time
--music = map (\t -> (1 + (distortion 30 (oscTriangle 13 t))) / 2 * oscSine 2300 t) time
-- http://audio.tutsplus.com/tutorials/production/essential-synthesis-part-2-classic-synth-pads/
music =
    take (2*44100) (map (\t -> (oscSawtooth (freq C4) t + oscSawtooth (freq C6 + 0.2) t) * 0.5 * oscSine 0.25 t) time)
    --take (2*44100) (map (\t -> (oscSawtooth (freq G3) t + oscSawtooth (freq G3 + 0.2) t) * 0.5 * oscSine 0.25 t) time) ++
    --take (2*44100) (map (\t -> (oscSawtooth (freq A3) t + oscSawtooth (freq A3 + 0.2) t) * 0.5 * oscSine 0.25 t) time) ++
    --take (2*44100) (map (\t -> (oscSawtooth (freq F3) t + oscSawtooth (freq F3 + 0.2) t) * 0.5 * oscSine 0.25 t) time)

-- TODO: 
--
--
--
--
--
--
--
--

main = pulseaudioOutput music
--main = waveOutput "test.wav" $ take (44100*30) music
