module Output
( waveOutput
, pcmOutput
, pcmOutput'
) where

import Types
import Data.WAVE

import qualified Data.Vector.Fusion.Stream.Monadic as S

import qualified Data.ByteString.Lazy as B

import Control.Monad.IO.Class (liftIO)

waveOutput :: String -> Int -> S.Stream IO Double -> IO ()
waveOutput filename length stream = do
    -- TODO: stream the wav file, seek and overwrite the length byte when finished.
    s <- S.toList stream
    putWAVEFile
        filename 
        $ WAVE (WAVEHeader 1 44100 16 (Just length)) $ map (\x -> [doubleToSample x]) $ take length s

pcmOutput :: [Double] -> IO ()
pcmOutput stream = do
    B.putStr $ B.concat $ map (to4Chars . toUInt16) stream
    where
        toUInt16 :: Double -> Int
        toUInt16 d = round $ (1 + max (-1) (min 1 d)) * (2 ^ 14 - 1)

        to4Chars :: Int -> B.ByteString
        to4Chars i = B.pack [ fromIntegral $ i `mod` 256, fromIntegral $ (i `div` 256) `mod` 256 ]
                
pcmOutput' :: S.Stream IO Double -> IO ()
pcmOutput' stream = do
    S.mapM_ (B.putStr . to4Chars . toUInt16) stream
    where
        toUInt16 :: Double -> Int
        toUInt16 d = round $ (1 + max (-1) (min 1 d)) * (2 ^ 14 - 1)

        to4Chars :: Int -> B.ByteString
        to4Chars i = B.pack [ fromIntegral $ i `mod` 256, fromIntegral $ (i `div` 256) `mod` 256 ]