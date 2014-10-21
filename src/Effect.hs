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

-- Subtracting two streams
(<-->) :: (Num a) => [a] -> [a] -> [a]
(<-->) = S.zipWith (-)
infixl 6 <-->

-- Adding constant
(<+->) :: (Num a) => [a] -> a -> [a]
(<+->) = flip $ (flip (S.zipWith (+))) . S.repeat
infixl 6 <+->

-- Raising to a power
(<***>) :: (Floating a) => [a] -> [a] -> [a]
(<***>) = S.zipWith (**)
infixl 8 <***>

-- Raising to a constant power
(<**->) :: (Floating a) => [a] -> a -> [a]
--(<**->) = flip $ (flip $ S.zipWith (**)) . S.repeat
(<**->) a b = S.zipWith (**) a (S.repeat b)
infixl 8 <**->

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

reverb wet decay xs =
    xs <*-> (1 - wet) <++>
    (echo decay 941 xs <*-> 0.1 <++>
    echo decay 1223 xs <*-> 0.1 <++>
    echo decay 1423 xs <*-> 0.1 <++>
    echo decay 2111 xs <*-> 0.1 <++>
    echo decay 2903 xs <*-> 0.1 <++>
    echo decay 3571 xs <*-> 0.1 <++>
    echo decay 4229 xs <*-> 0.1 <++>
    echo decay 5773 xs <*-> 0.1 <++>
    echo decay 7489 xs <*-> 0.1) <*-> wet

