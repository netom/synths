module Filter where

import Time
import Types
import Effect
import Data.Fixed
import Data.Complex
import Data.List
import qualified Data.List.Stream as S

z :: [Double] -> [Double]
z = (0 :)

feedback :: [Double] -> ([Double] -> [Double]) -> [Double] -> [Double]
feedback coeff func xs = output
    where
        output = func $ xs <--> (0 : output <**> coeff)

onepole :: [Double] -> [Double] -> [Double] -> [Double]
onepole a1s b0s xs = S.scanl (\y (a1, b0, x) -> b0 * x - a1 * y) 0 $ S.zip3 a1s b0s xs
-- onepole a1s b0s xs = S.scanl (\y (a1, b0, x) -> b0 * ()) 0 $ S.zip3 a1s b0s xs

fourpole :: [Double] -> [Double] -> [Double] -> [Double]
fourpole a1s b0s = onepole a1s b0s . onepole a1s b0s . onepole a1s b0s . onepole a1s b0s

-- See: Resonance Tuning for the digital Moog Filter

resofour :: [Double] -> [Double] -> [Double] -> [Double]
resofour resonances cfnorms = feedback ks $ fourpole a1s b0s
    where
        omegacs = cfnorms <*-> pi
        ss = map sin omegacs
        cs = map cos omegacs
        ts = map (\x -> tan $ (x - pi) / 4) omegacs
        a1s = ts <//> (ss <--> cs <**> ts)
        b0s = a1s <+-> 1
        g2s = b0s <**-> 2 <//> (a1s <**-> 2 <++> a1s <**> cs <*-> 2 <+-> 1)
        ks = resonances <**> g2s <**-> 2
