import Time
import Chord
import Types
import Effect
import Output
import Oscillator

--
-- Instruments
--

-- Reese bass: frequency, index
reese :: [Frequency] -> Frequency -> [Sample]
reese fs modf = mix (oscSawtooth fs) (oscSawtooth (map (+modf) fs) )

-- Simple synth pad
-- http://audio.tutsplus.com/tutorials/production/essential-synthesis-part-2-classic-synth-pads/
pad1 :: [Frequency] -> [Sample]
pad1 fs = reese fs 0.2

--
-- Tracks
--

-- Bass loop
bassloop1 = cycle ( (eID 4 $ reese (freqs A1) 2) ++
    (eID 2 $ reese (freqs C2) 2) ++
    (eID 2 $ reese (freqs D2) 3) )

padloop1 = cycle ( (eID 2 $ chord pad1 (freqs C4) tMaj) ++
    (eID 2 $ chord pad1 (freqs G3) tMaj) ++
    (eID 2 $ chord pad1 (freqs A3) tMin) ++
    (eID 2 $ chord pad1 (freqs F3) tMaj))

--
-- Assembly, master
--

music =
    --take (2*44100) (map (\t -> (oscSawtooth (freq C4) t + oscSawtooth (freq C6 + 0.2) t) * 0.5 * oscSine 0.25 t) time)
    --take (2*44100) (map (\t -> (oscSawtooth (freq G3) t + oscSawtooth (freq G3 + 0.2) t) * 0.5 * oscSine 0.25 t) time) ++
    --take (2*44100) (map (\t -> (oscSawtooth (freq A3) t + oscSawtooth (freq A3 + 0.2) t) * 0.5 * oscSine 0.25 t) time) ++
    --take (2*44100) (map (\t -> (oscSawtooth (freq F3) t + oscSawtooth (freq F3 + 0.2) t) * 0.5 * oscSine 0.25 t) time)
    --take (2*44100) $ oscSine $ map (\(a,b) -> a+b) $ zip (map ((*100) . sin) [0,0.0003..]) (freqs A4) -- siren
    --take (2*44100) $ oscSawtooth $ freqs A4
    padloop1

main = do
    pulseaudioOutput music
--main = waveOutput "test.wav" $ take (44100*30) music
