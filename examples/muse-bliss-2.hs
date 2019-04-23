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

arpeggiator :: [Frequency] -> Int -> [Frequency]
arpeggiator _ 0 = []
arpeggiator notes octaves = notes ++ arpeggiator (map (*2) notes) (octaves - 1)

musearp :: [Frequency] -> [Sample]
musearp notes = concat $ map (\f -> take 5150 $ ampEnvelope <**> resofour (repeat 0.1) filEnvelope ( oscSawtooth (repeat f) ) ) $ up ++ down
    where
        up = arpeggiator notes 3
        down = init $ tail $ reverse up
        ampEnvelope = eADSR 0.005 0.02 0.4 0.03 0.1
        filEnvelope = repeat 1

music =
    reverb 0.9 0.90 $ resofour (repeat 0.1) (repeat 0.05) $ cycle (
    (musearp $ doChord C2 tMin) ++
    (musearp $ doChord Bb1 tMaj) ++
    (musearp $ doChord F2 tMin) ++
    (musearp $ doChord C2 tMin) ++
    (musearp $ doChord Bb1 tMaj) ++
    (musearp $ doChord F2 tMin) ++
    (musearp $ doChord C2 tMin) ++
    (musearp $ doChord C2 tMin) )

main = do
    pcmOutput $ S.fromList music
