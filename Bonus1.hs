module Bonus1 where
  --First question's answer -------
dayOfWeek :: Integer -> Integer -> Integer -> Integer
dayOfWeek y m d = d+  floor (fromIntegral(13 *(selectMonth m)+1) / 5.0) + k (decreaseYear (selectMonth m) y) + floor(fromIntegral(k (decreaseYear (selectMonth m) y))/4) + (l (decreaseYear (selectMonth m) y))*5 `mod` 7
  where
    decreaseYear :: Integer -> Integer -> Integer
    decreaseYear m' y'
      |  m'== 13 = y'-1
      |  m'== 14 = y'-1
      | otherwise    = y'

    selectMonth :: Integer -> Integer
    selectMonth x
      | x==1         = 13
      | x==2         = 14
      | otherwise    = x

    k :: Integer-> Integer
    k k'= k'`mod` 100
    l :: Integer -> Integer
    l l'= div l' 100
--Fourth question's answer -------
leap :: Integer -> Bool
leap y = (y `mod`4 == 0) && (y `mod`100 /= 0) || (y `mod` 400 == 0)
selectMonth :: Integer -> Integer -> Integer
selectMonth m y
  | m==2         = if leap y then 29 else 28
  | m==4 || m == 6 || m == 9 || m == 11      = 30
  | otherwise    = 31
