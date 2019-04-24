import Time
import Chord
import Types
import Effect
import Filter
import Output
import Oscillator
import Data.Complex


import Streamly
import Streamly.Prelude ((|:), nil)
import qualified Streamly.Prelude as S

import Control.DeepSeq
import Control.Exception (evaluate)

--
-- Assembly, master
--

filterLfo = (oscSin (repeat 0.1)) <*-> 0.2 <+-> 0.3 <++> (oscSin (repeat 3)) <*-> 0.099

resonance :: SerialT IO Double
resonance = S.repeat 0.1

tw  = 2700
q   = 5 * tw
h   = 2 * q
wh  = 2 * h
ft  = 4 * tw
ft2 = 2 * ft

mainIns note len = take len $ eADSR 0.01 0.01 0.9 0.01 0.1 <**> oscSawtooth (repeat $ freq note)

mainThemeRaw = samples `deepseq` S.take 441000 $ S.fromList $ cycle samples
    where
        samples =
            (  mainIns A3 q
            <> mainIns A4 q
            <> mainIns E4 h
            <> mainIns G4 h
            <> mainIns D4 h
            )

mainTheme = resofour' resonance (S.fromList filterLfo) mainThemeRaw

--mainTheme = S.fromList $ cycle (
--        mainIns A3 q ++
--        mainIns A4 q ++
--        mainIns E4 h ++
--        mainIns G4 h ++
--        mainIns D4 h
--    )

music :: SerialT IO Double
music = do
    x <- reverb' 0.7 0.95 $ echo' 0.2 ft2 mainTheme
    S.yield $ x * 4

--
-- walking | pv > /dev/null
--
-- Original:               160 - 170 KiB/s
-- Streamed echo & reverb: 200 - 220 KiB/s
main = do
    pcmOutput music
