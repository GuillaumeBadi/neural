
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

mT :: Matrix a -> Matrix a
mT = transpose

