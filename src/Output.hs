module Output
( waveOutput
, pcmOutput
) where

import Types
import Data.WAVE

import qualified Data.ByteString.Lazy as B

waveOutput :: String -> Int -> Stream -> IO ()
waveOutput filename length stream =
    --writeFile filename $ "RIFF" ++ (show $ map (\x -> [doubleToSample x]) $ take length stream)
    putWAVEFile
        filename 
        $ WAVE (WAVEHeader 1 44100 16 (Just length)) $ map (\x -> [doubleToSample x]) $ take length stream

pcmOutput :: Stream -> IO ()
pcmOutput stream = do
    B.putStr $ B.concat $ map (to4Chars . toUInt16) stream
    where
        toUInt16 :: Double -> Int
        toUInt16 d = round $ (1 + d) * (2 ^ 14 - 1)

        to4Chars :: Int -> B.ByteString
        to4Chars i = B.pack [ fromIntegral $ i `mod` 256, fromIntegral $ (i `div` 256) `mod` 256 ]
