{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}

module Filter where

import Time
import Types
import Effect
import Data.Fixed
import Data.Complex
import Data.List
import Data.IORef

import Streamly
import Streamly.Prelude ((.:))
import qualified Streamly.Prelude as S

z :: SerialT IO Double -> SerialT IO Double
z = (0 .:)

feedback coeff func xs = output
    where
        output = func $ xs <--> (0 : output <**> coeff)

feedback' :: SerialT IO Double -> (SerialT IO Double -> SerialT IO Double) -> SerialT IO Double -> SerialT IO Double
feedback' coeffs func xs = do
    y :: IORef Double <- S.yieldM $ newIORef 0

    --let tuples = zipSerially $ (,) <$> serially xs <*> serially coeffs
    --func $ (flip S.mapM) tuples $ \(x, coeff) -> do
    --    oldy <- readIORef y
    --    let newy = x - oldy * coeff
    --    writeIORef y newy
    --    return newy

    func $ (>>= S.yieldM) $ zipSerially $ do
        x <- serially xs
        coeff <- serially coeffs
        return $ do
            oldy <- readIORef y
            let newy = x - oldy * coeff
            writeIORef y newy
            return newy

onepole :: [Double] -> [Double] -> [Double] -> [Double]
onepole a1s b0s xs = scanl (\y (a1, b0, x) -> b0 * x - a1 * y) 0 $ zip3 a1s b0s xs
-- onepole a1s b0s xs = scanl (\y (a1, b0, x) -> b0 * ()) 0 $ zip3 a1s b0s xs

-- General one-pole digital filter
-- https://ccrma.stanford.edu/~jos/fp/One_Pole.html
onepole' :: SerialT IO Double -> SerialT IO Double -> SerialT IO Double -> SerialT IO Double
onepole' a1s b0s xs = do
    y :: IORef Double <- S.yieldM $ newIORef 0

    --let tuples = zipSerially $ (,,) <$> serially xs <*> serially a1s <*> serially b0s
    --(flip S.mapM) tuples $ \(x, a1, b0) -> do
    --    oldy <- readIORef y
    --    let newy = b0 * x - a1 * oldy
    --    writeIORef y newy
    --    return newy

    (>>= S.yieldM) $ zipSerially $ do
        x  <- serially xs
        a1 <- serially a1s
        b0 <- serially b0s
        return $ do
            oldy <- readIORef y
            let newy = b0 * x - a1 * oldy
            writeIORef y newy
            return newy

fourpole :: [Double] -> [Double] -> [Double] -> [Double]
fourpole a1s b0s = onepole a1s b0s . onepole a1s b0s . onepole a1s b0s . onepole a1s b0s

fourpole' :: SerialT IO Double -> SerialT IO Double -> SerialT IO Double -> SerialT IO Double
fourpole' a1s b0s = onepole' a1s b0s . onepole' a1s b0s . onepole' a1s b0s . onepole' a1s b0s

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

resofour' :: SerialT IO Double -> SerialT IO Double -> SerialT IO Double -> SerialT IO Double
resofour' resonances cfnorms = feedback' (serially ks) $ fourpole' (serially a1s) (serially b0s)
    where
        omegacs = (* pi) <$> cfnorms

        ss  = sin <$> omegacs

        cs  = cos <$> omegacs

        ts  = (\x -> tan $ (x - pi) / 4) <$> omegacs

        a1s = zipSerially $ (\t s c -> t / (s - c * t))
            <$> serially ts
            <*> serially ss
            <*> serially cs

        b0s = (+ 1) <$> a1s

        g2s = zipSerially $ (\b0 a1 c -> b0 * 2 / (a1 * 2 + a1 * c * 2 + 1))
            <$> serially b0s
            <*> serially a1s
            <*> serially cs

        ks  = zipSerially $ (\resonance g2 -> resonance / g2 * 2)
            <$> serially resonances
            <*> serially g2s