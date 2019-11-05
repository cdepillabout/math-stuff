module Lib where

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

matDiv :: [[a]] -> a -> [[a]]
matDiv = undefined

calcCovarMatrix :: Num a => [[a]] -> [[a]]
calcCovarMatrix mat =
  let mu = calcMatrixMu mat
      mu' = transpose mu
      mat' = transpose mat
      dMat = matSubtract mat mu
      dMat' = matSubtract mat' mu'
      res = dotProd dMat' dMat
      n = numSamples mat
  in matDiv res n
