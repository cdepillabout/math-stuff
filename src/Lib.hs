module Lib where

import Data.Foldable (foldl')

ffmap :: Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
ffmap f = fmap (fmap f)

-- | Return the number of rows of a matrix.
--
-- >>> numSamples [[1,2,3], [4,5,6], [7,8,9], [10,11,12]]
-- 4
--
-- >>> numSamples []
-- 0
numSamples :: Num a => [[a]] -> a
numSamples = fromIntegral . length

dotProd :: [[a]] -> [[a]] -> [[a]]
dotProd = undefined

-- | Subtract one matrix from another.
--
-- >>> matSubtract [[1,2,3],[4,5,6]] [[7,10,20],[1,3,10]]
-- [[-6,-8,-17],[3,2,-4]]
matSubtract :: forall a. Num a => [[a]] -> [[a]] -> [[a]]
matSubtract mat1 mat2 =
  let xs :: [([a], [a])] = zip mat1 mat2
      ys :: [[(a,a)]] = fmap (uncurry zip) xs
  in ffmap (uncurry (-)) ys

-- | Transpose a matrix.
--
-- >>> transpose [[1,2,3], [4,5,6], [7,8,9], [10,11,12]]
-- [[1,4,7,10],[2,5,8,11],[3,6,9,12]]
--
-- >>> transpose []
-- []
--
-- >>> transpose [[1,2,3]]
-- [[1],[2],[3]]
--
-- >>> transpose [[1],[2],[3]]
-- [[1,2,3]]
transpose :: [[a]] -> [[a]]
transpose = foldl' go []
  where
    go :: [[a]] -> [a] -> [[a]]
    go accum a = foldl' appendAt accum (zip [0..] a)

snoc :: [a] -> a -> [a]
snoc [] a = [a]
snoc (h:t) a = h : snoc t a

-- | Append a value to a given row in a matrix
--
-- >>> appendAt [[1,2,3],[4,5],[7,8,9]] (1, 6)
-- [[1,2,3],[4,5,6],[7,8,9]]
--
-- >>> appendAt [] (0, 6)
-- [[6]]
--
-- >>> appendAt [[],[1,2,3]] (0, 6)
-- [[6],[1,2,3]]
--
-- >>> appendAt [[],[1,2,3]] (0, 6)
-- [[6],[1,2,3]]
--
-- >>> appendAt [] (1,100)
-- *** Exception: appendAt: trying to append to the end of a row that doesn't exist
-- ...
appendAt :: [[a]] -> (Int, a) -> [[a]]
appendAt [] (0, a) = [[a]]
appendAt [] (n, a) = error "appendAt: trying to append to the end of a row that doesn't exist"
appendAt (h:t) (0, a) = snoc h a : t
appendAt (h:t) (n, a) = h : appendAt t (n - 1, a)

calcMatrixMu :: [[a]] -> [[a]]
calcMatrixMu = undefined

-- | Divide a matrix by a number.
--
-- >>> matDiv [ [10, 20, 30], [40, 50, 60], [70, 80, 90], [100, 110, 120] ] 10
-- [[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,9.0],[10.0,11.0,12.0]]
matDiv :: Fractional a => [[a]] -> a -> [[a]]
matDiv mat a = ffmap (/ a) mat

calcCovarMatrix :: Fractional a => [[a]] -> [[a]]
calcCovarMatrix mat =
  let mu = calcMatrixMu mat
      mu' = transpose mu
      mat' = transpose mat
      dMat = matSubtract mat mu
      dMat' = matSubtract mat' mu'
      res = dotProd dMat' dMat
      n = numSamples mat
  in matDiv res n
