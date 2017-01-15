module DNA (countNucleotides) where

import Data.Char

data Nucleotide = A | C | G | T deriving(Show, Eq, Read)

toNucleotide :: Char -> Nucleotide
toNucleotide c = read [toUpper c] :: Nucleotide 

countNucleotides :: String -> (Int, Int, Int, Int)
countNucleotides = foldl countF (0,0,0,0) . map toNucleotide
                     where countF (a, c, g, t) A = (a+1, c, g, t)
                           countF (a, c, g, t) C = (a, c+1, g, t)
                           countF (a, c, g, t) G = (a, c, g+1, t)
                           countF (a, c, g, t) T = (a, c, g, t+1)
