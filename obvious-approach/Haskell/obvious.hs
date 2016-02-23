module ObviousLists
where

naturalNumbers :: [Integer]
naturalNumbers = calcNaturalNumbers 1
                where
                    calcNaturalNumbers :: Integer -> [Integer]
                    calcNaturalNumbers n = n : (calcNaturalNumbers (n + 1))

slowFibs :: [Integer]
slowFibs = map fib naturalNumbers
           where
               fib :: Integer -> Integer
               fib 1 = 1
               fib 2 = 1
               fib n = (fib (n - 1)) + (fib (n - 2))

fastFibs :: [Integer]
fastFibs = [1, 1] ++ calcFibs (1, 1)
           where
               calcFibs :: (Integer, Integer) -> [Integer]
               calcFibs (x, y) = let
                                     next = x + y
                                 in
                                    next : calcFibs (y, next) 

arithmeticSequence :: Integer -> Integer -> [Integer]
arithmeticSequence a0 p = a0 : arithmeticSequence (a0 + p) p
