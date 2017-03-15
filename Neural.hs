
module Neural where

import Data.Matrix
import Data.Vector hiding (map)

test = fromLists [[1,2,3,4], [1,2,3,4]]
test2 = fromLists [[2,3,4,5], [2,3,4,5]]

mmap :: (Int -> Int -> a -> b) -> Matrix a -> Matrix b
mmap fn m = matrix (nrows m) (ncols m) $ \(i, j) -> fn i j (unsafeGet i j m)

hmult :: Num a => Matrix a -> Matrix a -> Matrix a
hmult m m' = matrix (nrows m) (ncols m) multE
  where multE (i, j) = unsafeGet i j m * unsafeGet i j m'

madd :: Num a => Matrix a -> Matrix a -> Matrix a
madd m m' = mmap add m
  where add i j e = (unsafeGet i j m') + e

smult :: Num a => a -> Matrix a -> Matrix a
smult = scaleMatrix

mmult :: Num a => Matrix a -> Matrix a -> Matrix a
mmult = multStd2

mT :: Matrix a -> Matrix a
mT = transpose

