import Time
import Chord
import Types
import Effect
import Filter
import Output

import Oscillator
import Data.Complex

import qualified Data.Vector.Fusion.Stream.Monadic as S

t :: Int
t = 2750

beep :: Note -> [Sample]
beep n = (take t $ oscSawtooth $ repeat $ freq n) ++ (take t $ silence)

music = 0.2 <-*> ( cycle $ concat $ map beep [ D3,D3, A3, D3,D3, Bb3, D3,D3, C4, D3,D3, A3, D3,D3, Bb3,G3 ] )

main = do
    pcmOutput' $ S.fromList music
