{-# LANGUAGE ScopedTypeVariables #-}
module Effect where

import Time
import Types
import Data.Fixed
import Data.List
import Data.IORef
import qualified Data.Sequence as Sq

import Streamly
import Streamly.Prelude ((|:), nil)
import qualified Streamly.Prelude as S

import qualified Data.Vector.Unboxed.Mutable as V

import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class (lift)

--
-- Some convenience operators
--

-- Scaling with constant
(<-*>) :: Double -> [Double] -> [Double]
(<-*>) = (flip (zipWith (*))) . repeat
infixl 7 <-*>

(<*->) :: [Double] -> Double -> [Double]
(<*->) = flip (<-*>)
infixl 7 <*->

-- Multiplying two streams (AM)
(<**>) :: [Double] -> [Double] -> [Double]
(<**>) = zipWith (*)
infixl 7 <**>

-- Scaling with constant (division)
(</->) :: [Double] -> Double -> [Double]
(</->) = flip $ (flip (zipWith (/))) . repeat
infixl 7 </->

-- Dividing two streams (AM)
(<//>) :: [Double] -> [Double] -> [Double]
(<//>) = zipWith (/)
infixl 7 <//>

-- Adding two streams (Mixing)
(<++>) :: [Double] -> [Double] -> [Double]
(<++>) = zipWith (+)
infixl 6 <++>

-- Subtracting two streams
(<-->) :: [Double] -> [Double] -> [Double]
(<-->) = zipWith (-)
infixl 6 <-->

-- Adding constant
(<+->) :: [Double] -> Double -> [Double]
(<+->) = flip $ (flip (zipWith (+))) . repeat
infixl 6 <+->

-- Raising to a power
(<***>) :: [Double] -> [Double] -> [Double]
(<***>) = zipWith (**)
infixl 8 <***>

-- Raising to a constant power
(<**->) :: [Double] -> Double -> [Double]
--(<**->) = flip $ (flip $ zipWith (**)) . repeat
(<**->) a b = zipWith (**) a (repeat b)
infixl 8 <**->

-- Amount, input, output
distortion :: Double -> Sample -> Sample
distortion amount i = max (-1) (min 1 (amount * i))

-- Quantize input to produce noise
quantize :: Double -> Sample -> Sample
quantize n i = fromIntegral (round (i * n)) / n

-- Mix N sample streams to one
mixN :: [[Sample]] -> [Sample]
mixN ss = foldr foldFunc [] (transpose ss)
    where
        foldFunc x xs = (sum x / fromIntegral (length x)) : xs

-- Echo
echo :: Double -> Int -> [Sample] -> [Sample]
echo q n xs = map snd $ scanl foldfunc (Sq.replicate n 0, 0) xs
    where
        foldfunc :: (Sq.Seq Sample, Sample) -> Sample -> (Sq.Seq Sample, Sample)
        foldfunc (cbuf, _) x = (cbuftail Sq.|> out, out)
            where
                cbuftail = Sq.drop 1 cbuf
                s = Sq.index cbuf 0
                out = x * 0.1 + s * q

echo' :: Double -> Int -> SerialT IO Double -> SerialT IO Double
echo' decay n stream = do
    buf :: V.MVector RealWorld Double <- S.yieldM $ V.new n
    pos :: IORef Int <- S.yieldM $ newIORef 0

    (flip S.mapM) stream $ \s -> do
        currentPos <- readIORef pos
        currentBufVal <- V.read buf currentPos

        let output = s * 0.1 + currentBufVal * decay

        V.write buf currentPos output
        writeIORef pos $ (currentPos + 1) `mod` n

        return output

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

reverb' :: Double -> Double -> SerialT IO Double -> SerialT IO Double
reverb' wet decay xs = zipSerially $ go
    <$> serially xs
    <*> serially (echo' decay 941 xs)
    <*> serially (echo' decay 1223 xs)
    <*> serially (echo' decay 1423 xs)
    <*> serially (echo' decay 2111 xs)
    <*> serially (echo' decay 2903 xs)
    <*> serially (echo' decay 3571 xs)
    <*> serially (echo' decay 4229 xs)
    <*> serially (echo' decay 5773 xs)
    <*> serially (echo' decay 7489 xs)
    where
        go x e1 e2 e3 e4 e5 e6 e7 e8 e9 = x * (1 - wet) + (0.1*e1+0.1*e2+0.1*e3+0.1*e4+0.1*e5+0.1*e6+0.1*e7+0.1*e8+0.1*e9) * wet