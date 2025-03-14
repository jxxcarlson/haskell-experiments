import Data.List (intercalate)

-- Convert a number to standard base k representation as a list of (coefficient, exponent) pairs
toBaseK :: Integer -> Integer -> [(Integer, Integer)]
toBaseK _ 0 = []
toBaseK k n = reverse $ go n 0
  where
    go 0 _ = []
    go num exp
      | r > 0     = (r, exp) : go q (exp + 1)
      | otherwise = go q (exp + 1)
      where (q, r) = num `divMod` k

-- Convert an exponent to hereditary base k notation
expandExponent :: Integer -> Integer -> String
expandExponent k 0 = "0"
expandExponent k exp = toHereditaryBaseK k exp

-- Convert a number to **hereditary** base k notation as a string
toHereditaryBaseK :: Integer -> Integer -> String
toHereditaryBaseK k n
  | n < k     = show n  -- If number is smaller than base, return it directly
  | otherwise = intercalate " + " $ map termToString (toBaseK k n)
  where
    termToString (coeff, 0) = show coeff
    termToString (1, exp) = kStr ++ "^" ++ expandExponent k exp
    termToString (coeff, exp) = show coeff ++ " * " ++ kStr ++ "^" ++ expandExponent k exp
    kStr = show k

-- Main function to test the hereditary base conversion
main :: IO ()
main = do
    let n = 18
        k = 2
    putStrLn $ "Hereditary Base " ++ show k ++ " of " ++ show n ++ " : " ++ toHereditaryBaseK k n
