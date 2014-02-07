import Time
import Chord
import Types
import Effect
import Filter
import Output
import Oscillator

--
-- Instruments
--

-- Reese bass: frequency, index
reese :: [Frequency] -> Frequency -> [Sample]
reese fs modf = oscSawtooth fs <*-> 0.5 <++> oscSawtooth (map (+modf) fs) <*-> 0.5

-- Simple synth pad
-- http://audio.tutsplus.com/tutorials/production/essential-synthesis-part-2-classic-synth-pads/
pad1 :: [Frequency] -> [Sample]
pad1 fs = reese fs 0.2

kick1 :: [Frequency] -> [Sample]
kick1 fs = oscSin (map (uncurry (+)) (zip fs contour))
    where
        kickTime = 0.2 -- in seconds
        kickLen =  round $ sampleRate * kickTime
        contourHeight = 100
        contourNorm = take kickLen (lfoSawtooth' (repeat (1/kickTime))) ++ repeat 0
        contour = map (uncurry (*)) $ zip contourNorm (repeat contourHeight)

--
-- Loops
--

-- Bass loop
bassloop1 =  fir (take 20 $ repeat 0.03) (take 19 $ repeat 0) $ cycle ( (eID 4 $ reese (freqs G2) 2) ++
    (eID 2 $ reese (freqs A2) 2) ++
    (eID 2 $ reese (freqs F2) 3) )

padloop1 = cycle ( (eID 2 $ chord pad1 (freqs C4) tMaj) ++
    (eID 2 $ chord pad1 (freqs G3) tMaj) ++
    (eID 2 $ chord pad1 (freqs A3) tMin) ++
    (eID 2 $ chord pad1 (freqs F3) tMaj))

kickloop1 = cycle (take 22050 (kick1 (repeat 0)))

--
-- Assembly, master
--

music =
    bassloop1 <*-> 0.25 <++> kickloop1 <*-> 0.7 <++> padloop1 <*-> 0.05

main = do
    pulseaudioOutput music
--main = waveOutput "test.wav" $ take (44100*30) music
