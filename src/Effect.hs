{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Effect where

import Time
import Types
import Data.Fixed
import Data.List
import Data.IORef
import qualified Data.Sequence as Sq

import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Fusion.Stream.Monadic as S

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

--echo' :: V.MVector RealWorld Double -> IORef Int -> Double -> Int -> S.Stream IO Double -> S.Stream IO Double
--echo' buf pos decay n stream = 

mkEcho' :: Int -> Double -> S.Stream IO Double -> IO (S.Stream IO Double)
mkEcho' n decay stream = do
    buf :: V.MVector RealWorld Double <- V.new n
    pos :: IORef Int <- newIORef 0
    return $ (flip S.mapM) stream $ \s -> do
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

zipWith10 :: Monad m => (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k)
    -> S.Stream m a -> S.Stream m b -> S.Stream m c -> S.Stream m d -> S.Stream m e
    -> S.Stream m f -> S.Stream m g -> S.Stream m h -> S.Stream m i -> S.Stream m j
    -> S.Stream m k
zipWith10 fn sa sb sc sd se sf sg sh si sj
    = S.zipWith
        (\(a, b, c, d, e) (f, g, h, i, j) -> fn a b c d e f g h i j)
        (S.zipWith5 (,,,,) sa sb sc sd se)
        (S.zipWith5 (,,,,) sf sg sh si sj)
 
{-# INLINE zipWith10 #-}

mkReverb' :: Double -> Double -> S.Stream IO Double -> IO (S.Stream IO Double)
mkReverb' wet decay xs = do
    e1 <- mkEcho' 941  decay xs
    e2 <- mkEcho' 1223 decay xs
    e3 <- mkEcho' 1423 decay xs
    e4 <- mkEcho' 2111 decay xs
    e5 <- mkEcho' 2903 decay xs
    e6 <- mkEcho' 3571 decay xs
    e7 <- mkEcho' 4229 decay xs
    e8 <- mkEcho' 5773 decay xs
    e9 <- mkEcho' 7489 decay xs

    return $ zipWith10 go xs e1 e2 e3 e4 e5 e6 e7 e8 e9

    where
        go x e1 e2 e3 e4 e5 e6 e7 e8 e9 = x * (1 - wet) + (0.1*e1+0.1*e2+0.1*e3+0.1*e4+0.1*e5+0.1*e6+0.1*e7+0.1*e8+0.1*e9) * wet