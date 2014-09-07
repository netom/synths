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
kickClick1 = drumBase oscTriangle 2500 0.99 0.2 <**> oscSin (repeat 80)

kickClick2 :: [Sample]
kickClick2 = noiseWhite <**> eExp 1 0.99

kickBass = drumBase oscSin 100 0.9998 0.3

kick2 :: [Sample]
kick2 = kickClick1 <*-> 0.3 <++> kickBass <*-> 0.7

kick3 :: [Sample]
kick3 = kickClick2 <*-> 0.2 <++> kickBass <*-> 0.8

kick4 :: [Sample]
kick4 = drumBase oscTriangle 1000 0.996 0.8 <*-> 0.2 <++> drumBase oscSin 110 0.9999 0.3 <*-> 0.8 -- Hááááárd!!!

pop :: Frequency -> [Sample]
pop f = oscSin (oscSin (repeat (f*2.123)) <*-> f <**> eExp 1 0.999 <++> repeat f) <**> eExp 1 0.999

openhat :: [Sample]
openhat = noiseWhite <**> eExp 1 0.9995

closedhat :: [Sample]
closedhat = noiseWhite <**> eExp 1 0.999

--
-- Basses
--

bass1 :: Time -> [Frequency] -> [Sample]
bass1 len fs = oscSawtooth (fs <*-> 2) <*-> 0.5 <**> eADSR 0.01 0.01 0.8 0.07 len

--
-- Loops
--

-- Bass loop
bassloop1 =  cycle ( (take 88200 $ reese (freqs G2) 2) ++
    (take 44100 $ reese (freqs A2) 2) ++
    (take 44100 $ reese (freqs F2) 3) )

bassloop2 = cycle $ take 5000 $ bass1 0 (freqs A0)

padloop1 = cycle ( (take 88200 $ chord pad1 (freqs C4) tMaj) ++
    (take 88200 $ chord pad1 (freqs G3) tMaj) ++
    (take 88200 $ chord pad1 (freqs A3) tMin) ++
    (take 88200 $ chord pad1 (freqs F3) tMaj))

padloop2 = cycle ( (take 88200 $ chord (pad2 1.6) (freqs C4) tMaj) ++
    (take 88200 $ chord (pad2 1.6) (freqs G3) tMaj) ++
    (take 88200 $ chord (pad2 1.6) (freqs A3) tMin) ++
    (take 88200 $ chord (pad2 1.6) (freqs F3) tMaj))

kickloop1 = cycle $ take 19600 $ kick1

kickloop2 = cycle $ take 23900 $ kick2 -- Shores 01

kickloop3 = cycle $ take 19600 $ kick3

kickloop4 = cycle $ take 20000 $ kick4

arpeggiator :: [Frequency] -> Int -> [Frequency]
arpeggiator _ 0 = []
arpeggiator notes octaves = notes ++ arpeggiator (map (*2) notes) (octaves - 1)

musearp :: [Frequency] -> [Sample]
musearp notes = concat $ map (\f -> take 5150 $ eADSR 0.01 0.01 0.5 0.03 0.1 <**> oscSin (repeat f) ) $ up ++ down
    where
        up = arpeggiator notes 3
        down = init $ tail $ reverse up

--
-- Assembly, master
--

filterLfo = (oscSin (repeat 0.1)) <*-> 0.2 <+-> 0.3 <++> (oscSin (repeat 3)) <*-> 0.099

resonance = repeat 11.5

tw  = 2700
q   = 5 * tw
h   = 2 * q
wh  = 2 * h
ft  = 4 * tw
ft2 = 2* ft

mainIns note len = take len $ eADSR 0.01 0.01 0.9 0.01 0.1 <**> oscSawtooth (repeat $ freq note)

mainTheme = resofour resonance filterLfo $ cycle (
        mainIns A3 q ++
        mainIns A4 q ++
        mainIns E4 h ++
        mainIns G4 h ++
        mainIns D4 h
    )

music =
    --reverb 0.6 $ resofour (repeat 0.25) (repeat 0.2) $ cycle (
    --(musearp $ doChord C4 tMin) ++
    --(musearp $ doChord Bb3 tMaj) ++
    --(musearp $ doChord F4 tMin) ++
    --(musearp $ doChord C4 tMin) ++
    --(musearp $ doChord Bb3 tMaj) ++
    --(musearp $ doChord F4 tMin) ++
    --(musearp $ doChord C4 tMin) ++
    --(musearp $ doChord C4 tMin) )

    -- Walking
    (reverb 0.5 $ echo 0.2 ft2 $ mainTheme) <*-> 5

main = do
    pcmOutput music
    --pcmOutput $ fourpole (repeat (-0.2)) (repeat 0.8) noiseWhite
    --pcmOutput $ onepole (repeat (-0.9)) (repeat 0.1) noiseWhite
    --pcmOutput $ resofour (repeat 0.8) (repeat 0.4) noiseWhite
    --print a1s
    --print b0s
    where
        cfnorms = [0.11]
        resonances = [0]
        a1s = cfnorms <--> repeat 1
        b0s = a1s <+-> 1
        ks = resonances <*-> 1.5
