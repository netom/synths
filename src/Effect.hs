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

-- Scaling with constant (division)
(</->) :: (Fractional a) => [a] -> a -> [a]
(</->) = flip $ (flip (S.zipWith (/))) . S.repeat
infixl 7 </->

-- Dividing two streams (AM)
(<//>) :: (Fractional a) => [a] -> [a] -> [a]
(<//>) = S.zipWith (/)
infixl 7 <//>

-- Adding two streams (Mixing)
(<++>) :: (Num a) => [a] -> [a] -> [a]
(<++>) = S.zipWith (+)
infixl 6 <++>

-- Adding constant
(<+->) :: (Num a) => [a] -> a -> [a]
(<+->) = flip $ (flip (S.zipWith (+))) . S.repeat
infixl 6 <+->

-- Amount, input, output
distortion :: Double -> Sample -> Sample
distortion amount i = max (-1) (min 1 (amount * i))

-- Quantize input to produce noise
quantize :: Double -> Sample -> Sample
quantize n i = fromIntegral (round (i * n)) / n

-- Mix N sample streams to one
mixN :: [[Sample]] -> [Sample]
mixN ss = S.foldr foldFunc [] (S.transpose ss)
    where
        foldFunc x xs = (S.sum x / fromIntegral (S.length x)) : xs

-- Echo
echo :: Double -> Int -> [Sample] -> [Sample]
echo q n xs = S.map snd $ S.scanl foldfunc (Sq.replicate n 0, 0) xs
    where
        foldfunc :: (Sq.Seq Sample, Sample) -> Sample -> (Sq.Seq Sample, Sample)
        foldfunc (cbuf, _) x = (cbuftail Sq.|> out, out)
            where
                cbuftail = Sq.drop 1 cbuf
                s = Sq.index cbuf 0
                out = x * 0.1 + s * q

reverb wet xs =
    xs <*-> wet <++>
    echo 0.99 941 xs <*-> 0.1 <++>
    echo 0.99 1223 xs <*-> 0.1 <++>
    echo 0.99 1423 xs <*-> 0.1 <++>
    echo 0.99 2111 xs <*-> 0.1 <++>
    echo 0.99 2903 xs <*-> 0.1 <++>
    echo 0.99 3571 xs <*-> 0.1 <++>
    echo 0.99 4229 xs <*-> 0.1

