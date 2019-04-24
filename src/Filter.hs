{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}

module Filter where

import Time
import Types
import Effect
import Data.Fixed
import Data.Complex
import Data.List
import Data.IORef

import qualified Data.Vector.Fusion.Stream.Monadic as S

z :: S.Stream IO Double -> S.Stream IO Double
z = S.cons 0

feedback coeff func xs = output
    where
        output = func $ xs <--> ((0 : output) <**> coeff)
     
feedback' :: S.Stream IO Double -> (S.Stream IO Double -> S.Stream IO Double) -> S.Stream IO Double -> S.Stream IO Double
feedback' coeffs func xs = outs
    where
        outs = func $ S.zipWith3 (\out' x coeff -> x - out' * coeff) (z outs) xs coeffs

onepole :: [Double] -> [Double] -> [Double] -> [Double]
onepole a1s b0s xs = scanl (\y (a1, b0, x) -> b0 * x - a1 * y) 0 $ zip3 a1s b0s xs

-- General one-pole digital filter
-- https://ccrma.stanford.edu/~jos/fp/One_Pole.html
onepole' :: S.Stream IO Double -> S.Stream IO Double -> S.Stream IO Double -> S.Stream IO Double
onepole' a1s b0s xs = S.scanl (\y (a1, b0, x) -> b0 * x - a1 * y) 0 $ S.zipWith3 (,,) a1s b0s xs

mkOnePole' :: S.Stream IO Double -> S.Stream IO Double -> S.Stream IO Double -> IO (S.Stream IO Double)
mkOnePole' a1s b0s xs = do
    y :: IORef Double <- newIORef 0

    return $ (flip S.mapM) (S.zipWith3 (,,) a1s b0s xs) $ \(a1, b0, x) -> do
        oldy <- readIORef y
        let newy = b0 * x - a1 * oldy
        writeIORef y newy
        return newy

fourpole :: [Double] -> [Double] -> [Double] -> [Double]
fourpole a1s b0s = onepole a1s b0s . onepole a1s b0s . onepole a1s b0s . onepole a1s b0s

fourpole' :: S.Stream IO Double -> S.Stream IO Double -> S.Stream IO Double -> S.Stream IO Double
fourpole' a1s b0s = onepole' a1s b0s . onepole' a1s b0s . onepole' a1s b0s . onepole' a1s b0s

mkFourPole' :: S.Stream IO Double -> S.Stream IO Double -> S.Stream IO Double -> IO (S.Stream IO Double)
mkFourPole' a1s b0s xs
    =   mkOnePole' a1s b0s xs
    >>= (mkOnePole' a1s b0s)
    >>= (mkOnePole' a1s b0s)
    >>= (mkOnePole' a1s b0s)

-- See: Resonance Tuning for the digital Moog Filter
-- http://www.rs-met.com/documents/dsp/ResonanceTuningForTheDigitalMoogFilter.pdf
resofour :: [Double] -> [Double] -> [Double] -> [Double]
resofour resonances cfnorms = feedback ks $ fourpole a1s b0s
    where
        omegacs = cfnorms <*-> pi
        ss  = map sin omegacs
        cs  = map cos omegacs
        ts  = map (\x -> tan $ (x - pi) / 4) omegacs
        a1s = ts <//> (ss <--> cs <**> ts)
        b0s = a1s <+-> 1
        g2s = b0s <**-> 2 <//> (a1s <**-> 2 <++> a1s <**> cs <*-> 2 <+-> 1)
        ks  = resonances <//> g2s <**-> 2

resofour' :: S.Stream IO Double -> S.Stream IO Double -> S.Stream IO Double -> S.Stream IO Double
resofour' resonances cfnorms = feedback' ks $ fourpole' a1s b0s
    where
        omegacs = (* pi) <$> cfnorms

        ss  = sin <$> omegacs

        cs  = cos <$> omegacs

        ts  = (\x -> tan $ (x - pi) / 4) <$> omegacs

        a1s = S.zipWith3 (\t s c -> t / (s - c * t)) ts ss cs

        b0s = (+ 1) <$> a1s

        g2s = S.zipWith3 (\b0 a1 c -> b0 * 2 / (a1 * 2 + a1 * c * 2 + 1)) b0s a1s cs

        ks  = S.zipWith (\resonance g2 -> resonance / g2 * 2) resonances g2s