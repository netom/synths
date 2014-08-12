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

-- bassloop1 = cycle $ take 5975 $ bass1 0 (freqs C1)
-- Bass loop
bassloop1 =  cycle ( (take 88200 $ reese (freqs G2) 2) ++
    (take 44100 $ reese (freqs A2) 2) ++
    (take 44100 $ reese (freqs F2) 3) )

bassloop2 = cycle $ take 5000 $ bass1 0 (freqs A0)

bassloop3 =  cycle (
    (take 10000 $ bassFiltered 0.09 (freqs D1)) ++
    (take 10000 $ bassFiltered 0.09 (freqs B2)) ++
    (take 10000 $ bassFiltered 0.09 (freqs D1)) ++
    (take 10000 $ bassFiltered 0.09 (freqs D1)) ++
    (take 10000 $ bassFiltered 0.09 (freqs D2)) )

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

-- TODO: try difference lists?
poploop = cycle (
    (concat $ replicate 3 $
    (take 10000 $ pop (freq A4)) ++
    (take 20000 $ pop (freq C5)) ++
    (take 10000 $ pop (freq A4)) ++
    (take 10000 $ pop (freq B4)) ++
    (take 5000 $ pop (freq A4)) ++
    (take 15000 $ pop (freq B4)) ++
    (take 10000 $ pop (freq G4)) )++
    --
    (take 10000 $ pop (freq A4)) ++
    (take 20000 $ pop (freq C5)) ++
    (take 10000 $ pop (freq A4)) ++
    (take 10000 $ pop (freq B4)) ++
    (take 5000 $ pop (freq A4)) ++
    (take 10000 $ pop (freq B4)) ++
    (take 5000 $ pop (freq C5)) ++
    (take 5000 $ pop (freq D5)) ++
    (take 5000 $ pop (freq E5)) )

-- Bassline with shaped filter
bassFiltered :: Time -> [Frequency] -> [Sample]
bassFiltered len fs = lp303' (repeat 0.0007) (repeat 0.3) (eLinear 0.8 0.3 (len / 2)) $ oscSawtooth fs <**> eADSR a d s r len
    where
        a = 0.02
        d = 0.02
        s = 0.7
        r = 0.2
        fullLen = len + a + d + r


arpeggiator :: [Frequency] -> Int -> [Frequency]
arpeggiator _ 0 = []
arpeggiator notes octaves = notes ++ arpeggiator (map (*2) notes) (octaves - 1)

musearp :: [Sample]
musearp = concat $ map (\f -> take 5000 $ oscTriangle $ repeat f ) $ cycle $ up ++ down
    where
        up = arpeggiator [freq C4, freq Eb4, freq G4] 3
        down = init $ tail $ reverse up

--
-- Assembly, master
--

filterLfo = oscSin (repeat 4) <*-> 0.39 <+-> 0.61

resonance = repeat 0.8

music =
    --kickloop2 <*-> 0.6 <++> bassloop1 <*-> 0.4
    --bassloop1 <*-> 0.25 <++> kickloop1 <*-> 0.7 <++> padloop1 <*-> 0.05
    --fir coeffs [0|i<-[1..ntaps - 1]] padloop2
    -- padloop2
    --kickloop2 <*-> 0.6 <++> bassloop2 <*-> 0.4
    --where
    --    ntaps = 64
    --    coeffs = fDesign $ take ntaps $ [1,1] ++ [0,0..]
    --kickloop4
    --pop 120
    --(take 160000 kickloop4) <*-> 0.5 ++ (poploop <*-> 0.5 <++> kickloop4 <*-> 0.5)
    --reverb poploop
    --poploop
    -- lp303' : z hpf q fc x
    --lp303' (0, 0, 0, 0, 0) (repeat 0) (oscSin (repeat 0.3) </-> 3 <+-> 0.5) (oscSin (repeat 0.2) </-> 3 <+-> 0.5) (oscSawtooth (repeat 110))
    --lp303' (0, 0, 0, 0, 0) (oscSin (repeat 0.3) </-> 2 <+-> 0.5) (repeat 0) (repeat 0.5) (oscSawtooth (repeat 110))
    --lp303' (0, 0, 0, 0, 0) (repeat 0.5) (repeat 0) (oscSin (repeat 0.3) </-> 3 <+-> 0.5) (oscSawtooth (repeat 110))
    --oscSawtooth (repeat 440)
    --lp303' (0, 0, 0, 0, 0) (repeat 0.0007) (repeat 0.8) (oscSin (repeat 0.6) <*-> 0.4 <+-> 0.6) (bassloop2)
    --lp303' (repeat 0.007) resonance filterLfo (oscSquare (repeat 80))
    --bassFiltered 0.2 (repeat 60)
    --bassloop3
    musearp

main = do
    --pulseaudioOutput $ take (44100 * 30) music
    pcmOutput music
    --pulseaudioOutput $ replicate (44100 * 30) 0
    --waveOutput "test.wav" (44100 * 30) $ music
    --waveOutput "test.wav" (44100 * 30) $ repeat 0
    --pcmOutput [-1]
