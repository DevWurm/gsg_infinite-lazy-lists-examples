module UnintuitiveLists (
        naturalNumbers,
        fibs,
        arithmeticSequence
) where

naturalNumbers :: [Integer]
naturalNumbers = 1 : map (+ 1) naturalNumbers

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) (fibs) (tail fibs)

arithmeticSequence :: Integer -> Integer -> [Integer]
arithmeticSequence a0 p = a0 : map (+ p) (arithmeticSequence a0 p)
