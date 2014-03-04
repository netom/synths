module Effect where

import Time
import Types
import Data.Fixed
import qualified Data.List.Stream as S
import qualified Data.Sequence as Sq

--
-- Some convenience operators
--

-- Scaling with constant
(<*->) :: (Fractional a) => [a] -> a -> [a]
(<*->) = flip $ (flip (S.zipWith (*))) . S.repeat
infixl 7 <*->

-- Multiplying two streams (AM)
(<**>) :: (Num a) => [a] -> [a] -> [a]
(<**>) = S.zipWith (*)
infixl 7 <**>

-- Adding two streams (Mixing)
(<++>) :: (Num a) => [a] -> [a] -> [a]
(<++>) = S.zipWith (+)
infixl 6 <++>

-- Amount, input, output
distortion :: Double -> Sample -> Sample
distortion amount i = max (-1) (min 1 (amount * i))

-- Quantize input to produce noise
quantize :: Double -> Sample -> Sample
quantize n i = fromIntegral (round (i * n)) / n

-- Mix N sample streams to one
mixN :: [[Sample]] -> [Sample]
mixN ss = S.sum heads / l : mixN tails
    where
        heads = S.map head ss
        tails = S.map tail ss
        l = fromIntegral $ S.length ss

-- Echo
echo :: Int -> [Sample] -> [Sample]
echo n xs = S.map snd $ S.scanl foldfunc (Sq.replicate n 0, 0) xs
    where
        foldfunc :: (Sq.Seq Sample, Sample) -> Sample -> (Sq.Seq Sample, Sample)
        foldfunc (cbuf, _) x = (cbuftail Sq.|> out, out)
            where
                cbuftail = Sq.drop 1 cbuf
                s = Sq.index cbuf 0
                out = x * 0.1 + s * 0.99

reverb xs =
    xs <*-> 0.2 <++>
    echo 941 xs <*-> 0.1 <++>
    echo 1223 xs <*-> 0.1 <++>
    echo 1423 xs <*-> 0.1 <++>
    echo 2111 xs <*-> 0.1 <++>
    echo 2903 xs <*-> 0.1 <++>
    echo 3571 xs <*-> 0.1 <++>
    echo 4229 xs <*-> 0.1

