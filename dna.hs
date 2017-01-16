module DNA (
countNucleotides, 
reverseComplement, 
gcContent,
countMutations
) where

import Data.Char

countNucleotides :: String -> (Int, Int, Int, Int)
countNucleotides = foldl countF (0,0,0,0) . map toUpper
                     where countF (a, c, g, t) 'A' = (a+1, c, g, t)
                           countF (a, c, g, t) 'C' = (a, c+1, g, t)
                           countF (a, c, g, t) 'G' = (a, c, g+1, t)
                           countF (a, c, g, t) 'T' = (a, c, g, t+1)
                           countF _ _ = error "Unrecognized nucleotide."

complement :: Char -> Char
complement 'A' = 'T'
complement 'T' = 'A'
complement 'C' = 'G'
complement 'G' = 'C'
complement _ = error "Unrecognized Nucleotide."

reverseComplement :: String -> String
reverseComplement = reverse . map complement

gcContent :: (Foldable t, Fractional a) => t Char -> a
gcContent s = (fromIntegral $ foldl countFunc 0 s) / (fromIntegral $ length s)
              where countFunc acc c = if c `elem` ['C', 'G'] then acc + 1 else acc

countMutations :: String -> String -> Int
countMutations s1 s2 = foldl (\acc (a, b) -> if a /= b then acc + 1 else acc) 0 $ zip s1 s2
