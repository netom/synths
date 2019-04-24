import Time
import Chord
import Types
import Effect
import Filter
import Output
import Oscillator
import Data.Complex

import qualified Data.Vector.Fusion.Stream.Monadic as S

--
-- Assembly, master
--

filterLfo = (oscSin (repeat 0.1)) <*-> 0.2 <+-> 0.3 <++> (oscSin (repeat 3)) <*-> 0.099

resonance :: [Double]
resonance = repeat 0.1

resonance' :: S.Stream IO Double
resonance' = S.fromList $ repeat 0.1

tw  = 2700
q   = 5 * tw
h   = 2 * q
wh  = 2 * h
ft  = 4 * tw
ft2 = 20 * ft

mainIns note len = take len $ eADSR 0.01 0.01 0.9 0.01 0.1 <**> oscSawtooth (repeat $ freq note)

mainTheme = resofour resonance filterLfo $ cycle (
        mainIns A3 q ++
        mainIns A4 q ++
        mainIns E4 h ++
        mainIns G4 h ++
        mainIns D4 h
    )

mainThemeRaw' = S.fromList $ cycle samples
    where
        samples =
            (  mainIns A3 q
            <> mainIns A4 q
            <> mainIns E4 h
            <> mainIns G4 h
            <> mainIns D4 h
            )

mainTheme' = resofour' resonance' (S.fromList filterLfo) mainThemeRaw'
mainTheme'' = fourpole' resonance' (S.fromList filterLfo) mainThemeRaw'

mkMainTheme''' = do
    onePole <- mkFourPole' resonance' (S.fromList filterLfo) mainThemeRaw'
    return onePole

music :: [Double]
music = (reverb 0.7 0.95 $ echo 0.2 ft2 $ mainTheme) <*-> 4
    
mkMusic' :: IO (S.Stream IO Double)
mkMusic' = do
    mainTheme''' <- mkMainTheme'''
    echo <- mkEcho' ft2 0.2 mainTheme'''
    reverb <- mkReverb' 0.7 0.95 echo
    return $ (* 4) <$> reverb

--
-- walking | pv > /dev/null
--
-- Original:               160 - 170 KiB/s
-- Streamed echo & reverb: 200 - 220 KiB/s
main = do
    music' <- mkMusic'
    pcmOutput' music'
