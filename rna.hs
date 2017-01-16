module RNA (fromDNA) where

import Data.Char

fromDNA :: String -> String
fromDNA = map (\c -> if c == 'T' then 'U' else c) . map toUpper
