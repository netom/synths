import Time
import Chord
import Types
import Effect
import Filter
import Output
import Oscillator
import Data.Complex

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

pad2 :: Time -> [Frequency] -> [Sample]
pad2 len fs = reese fs 0.2 <**> eADSR 0.02 0.02 0.7 0.1 len

kick1 :: [Sample]
kick1 = oscSin $ eLinear 120 0 0.2
    where
        kickTime = 0.2 -- in seconds
        kickLen =  round $ sampleRate * kickTime
        contourHeight = 120
        contourNorm = take kickLen (lfoSawtooth' (repeat (1/kickTime))) ++ repeat 0
        contour = map (uncurry (*)) $ zip contourNorm (repeat contourHeight)

drumBase :: ([Frequency] -> [Sample]) -> Frequency -> Frequency -> Time -> [Sample]
drumBase osc fStart fDecay length = osc fKick <**> eLinear 1 0 length
    where
        fKick = eExp fStart fDecay

kickClick1 :: [Sample]
kickClick1 = drumBase oscTriangle 1000 0.99 0.2 <**> oscSin (repeat 80)

kickClick2 :: [Sample]
kickClick2 = noiseWhite <**> eExp 1 0.99

kickBass = drumBase oscSin 100 0.9998 0.3

kick2 :: [Sample]
kick2 = kickClick1 <*-> 0.2 <++> kickBass <*-> 0.6 <++> noiseWhite <**> eExp 0.2 0.95

kick3 :: [Sample]
kick3 = kickClick2 <*-> 0.2 <++> kickBass <*-> 0.8

--
-- Loops
--

-- Bass loop
bassloop1 =  cycle ( (take 88200 $ reese (freqs G2) 2) ++
    (take 44100 $ reese (freqs A2) 2) ++
    (take 44100 $ reese (freqs F2) 3) )

padloop1 = cycle ( (take 88200 $ chord pad1 (freqs C4) tMaj) ++
    (take 88200 $ chord pad1 (freqs G3) tMaj) ++
    (take 88200 $ chord pad1 (freqs A3) tMin) ++
    (take 88200 $ chord pad1 (freqs F3) tMaj))

padloop2 = cycle ( (take 88200 $ chord (pad2 1.6) (freqs C4) tMaj) ++
    (take 88200 $ chord (pad2 1.6) (freqs G3) tMaj) ++
    (take 88200 $ chord (pad2 1.6) (freqs A3) tMin) ++
    (take 88200 $ chord (pad2 1.6) (freqs F3) tMaj))

kickloop1 = cycle $ take 19600 $ kick1

kickloop2 = cycle $ take 19600 $ kick2

kickloop3 = cycle $ take 19600 $ kick3

--
-- Assembly, master
--

music =
    --bassloop1 <*-> 0.25 <++> kickloop1 <*-> 0.7 <++> padloop1 <*-> 0.05
    fir coeffs [0|i<-[1..ntaps - 1]] padloop2
    --padloop2
    --kickloop2
    where
        ntaps = 64
        coeffs = fDesign $ take ntaps $ [1,1] ++ [0,0..]

main = do
    --print $ fDesign $ take 128 $ [1,1,1,1] ++ [0,0..]
    pulseaudioOutput music
    --waveOutput "test.wav" $ take (44100*30) music
