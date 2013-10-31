module Types where

type Frequency = Double
type Time = Double
type Sample = Double
type Stream = [Sample]

data Note =
    C0 | Cs0 | D0 | Eb0 | E0 | F0 | Fs0 | G0 | Gs0 | A0 | Bb0 | B0 |
    C1 | Cs1 | D1 | Eb1 | E1 | F1 | Fs1 | G1 | Gs1 | A1 | Bb1 | B1 |
    C2 | Cs2 | D2 | Eb2 | E2 | F2 | Fs2 | G2 | Gs2 | A2 | Bb2 | B2 |
    C3 | Cs3 | D3 | Eb3 | E3 | F3 | Fs3 | G3 | Gs3 | A3 | Bb3 | B3 |
    C4 | Cs4 | D4 | Eb4 | E4 | F4 | Fs4 | G4 | Gs4 | A4 | Bb4 | B4 |
    C5 | Cs5 | D5 | Eb5 | E5 | F5 | Fs5 | G5 | Gs5 | A5 | Bb5 | B5 |
    C6 | Cs6 | D6 | Eb6 | E6 | F6 | Fs6 | G6 | Gs6 | A6 | Bb6 | B6 |
    C7 | Cs7 | D7 | Eb7 | E7 | F7 | Fs7 | G7 | Gs7 | A7 | Bb7 | B7 |
    C8 | Cs8 | D8 | Eb8 | E8 | F8 | Fs8 | G8 | Gs8 | A8 | Bb8 | B8
    deriving Enum

freq :: Note -> Double
freq note = 440 * ( (2 ** (1/12)) ** fromIntegral (fromEnum note - 57) )
