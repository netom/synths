module Output
( pulseaudioOutput
, waveOutput
) where

import Types
import Data.WAVE
import Sound.Pulse.Simple
import GHC.Float

chunks :: Int -> [a] -> [[a]]
chunks size list
    | length piece < size = [piece]
    | otherwise           = piece : chunks size (drop size list)
    where piece = take size list

pulseaudioOutput :: Stream -> IO ()
pulseaudioOutput stream = do
    s <- simpleNew Nothing "Synths" Play Nothing "Synths PCM output"
        (SampleSpec (F32 LittleEndian) 44100 1) Nothing Nothing

    let f32stream = map double2Float stream

    mapM_ (simpleWrite s) (chunks 1000 f32stream)

    simpleDrain s
    simpleFree s

waveOutput :: String -> Int -> Stream -> IO ()
waveOutput filename length stream =
    --writeFile filename $ "RIFF" ++ (show $ map (\x -> [doubleToSample x]) $ take length stream)
    putWAVEFile
        filename 
        $ WAVE (WAVEHeader 1 44100 16 (Just length)) $ map (\x -> [doubleToSample x]) $ take length stream
