-- Ex 1-4

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits n
  | n < 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther xs = let (next, remain) = splitAt (length xs - 2) xs in
  doubleEveryOther next ++ [head remain * 2, last remain]

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x)) `mod` 10 == 0

-- Ex 5

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c =
  step1 ++ step2 ++ step3
  where
    step1 = hanoi (n-1) a c b
    step2 = hanoi 1 a b c
    step3 = hanoi (n-1) c b a


main :: IO ()
main = do
  -- print (toDigits 1234)
  -- print (toDigitsRev 1234)
  -- print (toDigits 0)
  -- print (toDigits (-17))
  -- print (doubleEveryOther [8,7,6,5])
  -- print (doubleEveryOther [1,2,3])
  -- print (sumDigits [16,7,12,5])
  -- print (validate 4012888888881881)
  -- print (validate 4012888888881882)

  print (hanoi 2 "a" "b" "c")
