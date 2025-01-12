module Part3.Tasks where

import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = map f [n..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : (ff f $ f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq a = lst (take 10 $ repeat 0) a
    where
        lst arr [] = snd $ maximum $ zip arr [0..9]
        lst arr (x:xs) = lst (updArr arr x) xs
        updArr (x:xs) 0 = (x + 1):xs
        updArr arr x = updArr ((take (x `mod` 10) arr) ++ ((arr !! (x `mod` 10) + 1) : (drop (x `mod` 10 - 1) arr))) (x `div` 10)

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq lst = foldl (\acc x -> if x `elem` acc then acc else x:acc) [] lst

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = foldl (updGrok [] f) [] l
    where
        updGrok acc f [] x = (f x, [x]) : acc
        updGrok acc f ((y, n) : i) x = if f x /= y then updGrok ((y, n) : acc) f i x else i ++ ((y, x : n) : acc)