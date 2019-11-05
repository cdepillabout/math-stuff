module Lib where

ffmap :: Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
ffmap f = fmap (fmap f)

numSamples :: Num a => [[a]] -> a
numSamples = undefined

dotProd :: [[a]] -> [[a]] -> [[a]]
dotProd = undefined

matSubtract :: [[a]] -> [[a]] -> [[a]]
matSubtract = undefined

transpose :: [[a]] -> [[a]]
transpose = undefined

calcMatrixMu :: [[a]] -> [[a]]
calcMatrixMu = undefined

-- | Divide a matrix by a number.
--
-- >>> matDiv [ [10, 20, 30], [40, 50, 60], [70, 80, 90], [100, 110, 120] ] 10
-- [[1.0,2,3],[4,5,6],[7,8,9],[10,11,12]]
matDiv :: Fractional a => [[a]] -> a -> [[a]]
matDiv mat = ffmap (/ a) mat

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
