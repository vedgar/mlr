import Data.Char

-- Prvi zadatak

duljina [] = 0
duljina (a:as) = 1 + duljina as

wordHamming1 [] b = length b
wordHamming1 (a:as) [] = length (a:as)
wordHamming1 (a:as) (b:bs) | a == b = wordHamming1 as bs
                           | otherwise = 1 + wordHamming1 as bs

razliciti (a, b) | a == b = 0
                 | otherwise = 1
                 
wordHamming2 a b = sum (map razliciti (zip a b)) +
    abs (length a - length b)
    
-- Drugi zadatak

vigenere k ot = map zbroji (zip (cycle k) (izbaci (map toLower ot)))

zbroji (a, b) = broj2slovo (sum (map slovo2broj [a, b]) `mod` 26)

slovo2broj s = ord s - ord 'a'
broj2slovo n = chr (n + ord 'a')

izbaci = filter isAlpha

-- Treci zadatak

{- nubRight [] = []
nubRight (x:xs) = -}

nubLeft [] = []
nubLeft (x:xs) | x `elem` xs = nubLeft xs
               | otherwise = (x:nubLeft xs)

nubRight :: Eq a => [a] -> [a]
nubRight = reverse . nubLeft . reverse

-- Cetvrti zadatak

sumEven1 [] = 0
sumEven1 [x] = 0
sumEven1 (x:y:xs) = y + sumEven1 xs

sumEven2 list = sumEven' list 0
sumEven' [] ac = ac
sumEven' [x] ac = ac
sumEven' (x:y:xs) ac = sumEven' xs (ac+y)

