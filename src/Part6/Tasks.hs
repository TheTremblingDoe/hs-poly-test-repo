{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks (SparseMatrix(..), Matrix(matrixFromList, matrixW, matrixH, matrixE), eye, zero, multiplyMatrix, determinant) where

import Util (notImplementedYet)
import Data.Map

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
       matrixFromList :: [[Int]] -> mx
       matrixH :: mx -> Int
       matrixW :: mx -> Int
       matrixE :: mx -> Int -> Int -> Int
       matrixI :: mx -> Int -> Int -> Bool
       matrixI m i j = i >= 0 && j >= 0 && j < matrixH m && i < matrixW m
       mError :: mx -> ShowS -> a
       mError _ m = error $ (function . m) ""
              where
                     function = ("matrixE" ++)
       matrixBadI :: mx -> Int
       matrixBadI m = mError m output
              where
                     output = ("Invalid Ind" ++)
       matrixBadS :: mx -> mx
       matrixBadS m = mError m output
              where
                     output = ("Invalid Size" ++)

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
       matrixFromList [[x]] = x
       matrixFromList _ = matrixBadS 0
       matrixH _ = 1
       matrixW _ = 1
       matrixE mx 0 0 = mx
       matrixE mx _ _ = matrixBadI mx
instance Matrix [[Int]] where
       matrixFromList lst = lst
       matrixW [] = 0
       matrixW (x : xs) = length x
       matrixH = length
       matrixE mx i j
              | matrixI mx i j = mx !! i !! j
              | otherwise = matrixBadI mx
instance Matrix (SparseMatrix Int) where
       matrixFromList [] = SparseMatrix 0 0 empty
       matrixFromList (x : xs) = SparseMatrix (length x) (length xs + 1) (Data.Map.fromList $ toSparseMatrix [] 0 (x : xs))
              where
                     toSparseMatrix acc _ [] = acc
                     toSparseMatrix acc j (x : xs) = toSparseMatrix (toSparseJ acc 0 j x) (j + 1) xs
                     toSparseJ acc _ _ [] = acc
                     toSparseJ acc i j (y : ys) = toSparseJ (if y /= 0 then ((i, j), y) : acc else acc) (i + 1) j ys
       matrixH = sparseMatrixHeight
       matrixW = sparseMatrixWidth
       matrixE mx i j
              |matrixI mx i j = findWithDefault 0 (i, j) $ sparseMatrixElements mx
              |otherwise = matrixBadI mx
-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = matrixFromList [[if i == j then 1 else 0 | j <- [0 .. w - 1]] | i <- [0 .. w - 1]]
-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = matrixFromList [[0 | _ <- [0 .. w - 1]] | _ <- [0 .. h - 1]]
-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix lhv rhv
       | isVS lhv rhv = matrixFromList $ calc [] 0 lhv rhv
       | otherwise = errorBadS
       where
              calc acc j lhv rhv
                     | j == matrixH rhv = acc
                     | otherwise = calc (calcJ [] 0 j lhv rhv : acc) (j + 1) lhv rhv
              calcJ acc i j lhv rhv
                     | i == matrixW rhv = acc
                     | otherwise = calcJ (calcE i j lhv rhv : acc) (i + 1) j lhv rhv
              calcE i j lhv rhv = sum [matrixE lhv idx j * matrixE rhv i idx | idx <- [0 .. matrixH rhv - 1]]
              isVS lhv rhv = matrixW lhv == matrixH rhv
              errorBadS = error "In Multipy Invalid Matrix Size"
-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant mx
       | isVS mx = calcD [0 .. matrixH mx - 1] 0 mx
       | otherwise = errorBadS
       where
              calcD [i] j mx = matrixE mx i j
              calcD is j mx = sum [(-1) ^ k * matrixE mx i j * calcD (Prelude.filter (/= i ) is) (j + 1) mx | (k, i) <- zip [0..] is]
              isVS mx = matrixW mx == matrixH mx
              errorBadS = error "In Determinant Invalid Matrix Size"