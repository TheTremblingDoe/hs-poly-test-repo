module Part1.Tasks where

import Util(notImplementedYet)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x
    | abs x <= 2 * pi = hSin 0.0 16 x
    | otherwise = hSin 0.0 16 $ trunc x
    where
        hSin acc 0 x = acc + x
        hSin acc n x = hSin (acc + (((fromInteger $ hPow 1 (-1) n) / (fromInteger $ hFact 1 $ 2 * n + 1)) * (hPow 1.0 x $ 2 * n + 1))) (n - 1) x
        hPow acc _ 0 = acc
        hPow acc base exp = hPow (base * acc) base (exp - 1)
        hFact acc 0 = acc
        hFact acc n = hFact (n * acc) (n - 1)
        trunc x = x - 2 * pi * (fromInteger $ floor $ (x / (2 * pi)))

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x
    | abs x <= 2 * pi = hCos 0.0 16 x
    | otherwise = hCos 0.0 16 $ trunc x
    where
        hCos acc 0 _ = acc + 1.0
        hCos acc n x = hCos (acc + (((fromInteger $ hPow 1 (-1) n) / (fromInteger $ hFact 1 $ 2 * n)) * (hPow 1.0 x $ 2 * n))) (n - 1) x
        hPow acc _ 0 = acc
        hPow acc base exp = hPow (base * acc) base (exp - 1)
        hFact acc 0 = acc
        hFact acc n = hFact (n * acc) (n - 1)
        trunc x = x - 2 * pi * (fromInteger $ floor $ (x / (2 * pi)))

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a 0 = abs a
myGCD 0 b = abs b
myGCD a b = myGCD b $ a `mod` b

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect dd mm yy
    | dd <= 28 && mm == 2 = True
    | dd <= 30 && (mm == 4 || mm == 6 || mm == 9 || mm == 11) = True
    | dd <= 31 && (mm == 1 || mm == 3 || mm == 5 || mm == 7 || mm == 8 || mm == 10 || mm == 12) = True
    | dd <= 29 && mm == 2 = (yy `mod` 4 == 0 && yy `mod` 100 /= 0) || (yy `mod` 400 == 0)
    | otherwise = False

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow base exp = insidePow 1 base exp
    where
        insidePow n _ 0 = n
        insidePow n base exp = insidePow (n * base) base (exp - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 2 = True
isPrime n = check n $ ceiling $ sqrt $ fromInteger n
    where
        check _ 1 = True
        check n i = if n `mod` i /= 0 then check n $ i - 1 else False

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea p = (/2) $ abs $ foo 0.0 p $ head p
    where
        foo n [(xn, yn)] (x0, y0) = n + xn * y0 - x0 * yn
        foo n ((xi, yi) : (xj, yj) : p) p0 = foo (n + xi * yj - xj * yi) ((xj, yj):p) p0

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c
    | a <= b && b <= c = genAns a b c
    | a <= b && c <= a = genAns c a b
    | a <= c && b <= a = genAns b a c
    | a <= c && c <= b = genAns a c b
    | b <= a && c <= b = genAns c b a
    | b <= c && c <= a = genAns b c a
    where
        genAns a b c
            | a + b < c = -1
            | a * a + b * b < c * c = 0
            | a * a + b * b > c * c = 1
            | a * a + b * b == c * c = 2
