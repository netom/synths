{-# Language RecordWildCards #-}
{-# Language ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.ByteArray
import Data.IORef
import Data.Word
import Foreign.Ptr
import Foreign.Storable

import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed.Mutable as V

import Lib

-- Mutable Double Vector with ever increasing index
-- The index variable is 64 bit, the music can play
-- for up to about 6.6 million years assuming a
-- sample rate of 44100Hz.
--
-- GHCI > fromIntegral (maxBound :: Int) / 44100 / 86400 / 365 / 1000000
--
data DV = DV
    { -- The double vector
      dvV :: V.MVector (PrimState IO) Double
      -- Index start value
    , dvI :: IORef Int
    }

-- Increase the index start value by the length of the buffer
stepDVI :: DV -> IO ()
stepDVI DV{..} = atomicModifyIORef' dvI (\i -> (i + V.length dvV, ()))

-- Execute an action for each of the elements of the buffer
bmapM_ :: (Double -> IO ()) -> DV -> IO ()
bmapM_ a DV{..} = forM_ [0..V.length dvV - 1] $ \i -> V.unsafeRead dvV i >>= a

-- Map Monadic index, index start value supplied as the first argument
bmapMI' :: Int -> (Int -> IO Double) -> DV -> IO ()
bmapMI' i' a DV{..} = forM_ [i'..i' + V.length dvV - 1] $ \i -> a i >>= V.unsafeWrite dvV (i - i')

-- Map Monadic index
bmapMI :: (Int -> IO Double) -> DV -> IO ()
bmapMI a dv@DV{..} = do
    i' <- readIORef dvI
    bmapMI' i' a dv

-- Map Monadic value
bmapMV :: (Double -> IO Double) -> DV -> IO ()
bmapMV a dv@DV{..} = do
    i' <- readIORef dvI
    bmapMI' i' (\i -> V.unsafeRead dvV (i - i') >>= a) dv

-- Map Pure index
bmapI :: (Int -> Double) -> DV -> IO ()
bmapI f = bmapMI (return . f)

-- Map Pure value
bmapV :: (Double -> Double) -> DV -> IO ()
bmapV f = bmapMV (return . f)

newDV :: Int -> Int -> IO DV
newDV n i = do
    dvV <- V.new n
    dvI <- newIORef i
    return DV{..}

runDV :: DV -> (DV -> IO ()) -> IO ()
runDV dv a = a dv

runDVForever :: Int -> (DV -> IO ()) -> IO ()
runDVForever n a = do
    dv <- newDV 1024 0
    forever $ do
        runDV dv a
        stepDVI dv

runDVFor :: Int -> Int -> (DV -> IO ()) -> IO ()
runDVFor n steps a = do
    dv <- newDV n 0
    forM_ [1..steps `div` n] $ \i -> do
        runDV dv a
        stepDVI dv
    i <- readIORef $ dvI dv
    dv' <- newDV (steps `mod` n) i
    runDV dv' a

pcmOutput :: DV -> IO ()
pcmOutput dv@DV{..} = do
    let l = V.length dvV

    bs <- alloc (l * 2) $ \(p :: Ptr Word16) -> do
        forM_ [0..l - 1] $ \i -> do
            e <- V.unsafeRead dvV i
            pokeElemOff p i $ toW16 e 

    B.putStr bs

    where
        toW16 :: Double -> Word16
        toW16 d = round $ (1 + max (-1) (min 1 d)) * (2 ^ 14 - 1)

-- Sample rate
sr :: Double
sr = 44100

-- Index time to normalized time
i2nt :: Int -> Double
i2nt i = fromIntegral i / sr

-- Index time to radial time
i2rt :: Double -> Double
i2rt t = 2 * pi * t

-- Frequency -> Time -> Sample
sineOscillator :: Double -> Double -> Double
sineOscillator f t = 0.2 * sin (f * i2rt t)

-- Play the output with:
-- stack exec buf-exe | play -r 44100 -b 16 -c 1 --endian little -t raw -e unsigned-integer -
--
-- Measure performance with:
-- stack exec buf-exe | pv > /dev/null
main = do
    putStrLn "Hello"
    runDVFor 1024 44100 $ \dv -> do
        bmapI (sineOscillator $ 440) dv
        pcmOutput dv
