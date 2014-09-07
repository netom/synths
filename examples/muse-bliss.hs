import Time
import Chord
import Types
import Effect
import Filter
import Output
import Oscillator
import Data.Complex

arpeggiator :: [Frequency] -> Int -> [Frequency]
arpeggiator _ 0 = []
arpeggiator notes octaves = notes ++ arpeggiator (map (*2) notes) (octaves - 1)

musearp :: [Frequency] -> [Sample]
musearp notes = concat $ map (\f -> take 5150 $ eADSR 0.005 0.02 0.4 0.03 0.1 <**> oscSin (repeat f) ) $ up ++ down
    where
        up = arpeggiator notes 3
        down = init $ tail $ reverse up

music =
    reverb 0.9 $ resofour (repeat 0) (repeat 0.15) $ cycle (
    (musearp $ doChord C4 tMin) ++
    (musearp $ doChord Bb3 tMaj) ++
    (musearp $ doChord F4 tMin) ++
    (musearp $ doChord C4 tMin) ++
    (musearp $ doChord Bb3 tMaj) ++
    (musearp $ doChord F4 tMin) ++
    (musearp $ doChord C4 tMin) ++
    (musearp $ doChord C4 tMin) )

main = do
    pcmOutput music
