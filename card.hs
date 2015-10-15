toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    |  n == 0 = [0]
    |  n >= 0 = (n `mod` 10) : ( if m == 0 then [] else toDigitsRev m)
    | otherwise = error " error lower than 0"
        where m =  n `div` 10

toDigits :: Integer -> [Integer]
toDigits n
    | n == 0  = [0]
    | n < 0 = error " error lower than 0 " 
    | otherwise = reverse $ toDigitsRev n


doubleSecond :: [Integer] -> [Integer]
doubleSecond [] = []
doubleSecond [x] = [x]
doubleSecond (x:y:xs) = x : (*) y 2 : [] ++ doubleSecond xs

sumDigits :: [Integer] -> Integer
sumDigits n = sum $ map(\x -> sum $ toDigits x) n

isValid :: Integer -> Bool
isValid n = r == 0
    where r = (sumDigits $ doubleSecond $ toDigitsRev n) `mod` 10

