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

-- | Calculate the dot product of two vectors.
--
-- >>> dotProdVec [1,2,3] [4,5,6]
-- 32
--
-- >>> dotProdVec [] [1,2,3]
-- *** Exception: dotProdVec: can't take a dot product when the first arg is an empty list.
-- ...
--
-- >>> dotProdVec [1,2,3] []
-- *** Exception: dotProdVec: can't take a dot product when the second arg is an empty list.
-- ...
--
-- >>> dotProdVec [1,2,3] [4,5]
-- *** Exception: dotProdVec: it doesn't make sense to take a dot product of vectors of two different lengths
-- ...
dotProdVec :: Num a => [a] -> [a] -> a
dotProdVec [] _ = error "dotProdVec: can't take a dot product when the first arg is an empty list."
dotProdVec _ [] = error "dotProdVec: can't take a dot product when the second arg is an empty list."
dotProdVec v1 v2
  | length v1 /= length v2 =
  error "dotProdVec: it doesn't make sense to take a dot product of vectors of two different lengths"
  | otherwise = sum $ zipWith (*) v1 v2

-- | Calculate the number of rows in a  matrix.
--
-- >>> numRowsMat [[1,2],[3,4],[5,6]]
-- 3
--
-- >>> numRowsMat [[]]
-- 1
--
-- >>> numRowsMat []
-- 0
numRowsMat :: [[a]] -> Int
numRowsMat = length

-- | Calculate the number of columns in a matrix.
--
-- >>> numColsMat [[1,2],[3,4],[5,7]]
-- 2
--
-- >>> numColsMat [[]]
-- 0
--
-- >>> numColsMat []
-- 0
numColsMat :: [[a]] -> Int
numColsMat = length . transpose

-- | Calculate the dot product of two matricies.
--
-- >>> dotProdMat [[1,2],[3,4]] [[5,6],[7,8]]
-- [[19,22],[43,50]]
--
-- >>> dotProdMat [[0,3,1]] [[5,2],[0,2],[10,1]]
-- [[10,7]]
--
-- >>> dotProdMat [[3,9]] [[2],[-1]]
-- [[-3]]
--
-- >>> dotProdMat [[4],[6],[-1]] [[5,2]]
-- [[20,8],[30,12],[-5,-2]]
--
-- >>> dotProdMat [] []
-- []
--
-- >>> dotProdMat [[1,2,3]] [[5,2],[0,2]]
-- *** Exception: dotProdMat: number of columns of first matrix doesn't equal number of rows of second matrix
-- ...
dotProdMat :: forall a. Num a => [[a]] -> [[a]] -> [[a]]
dotProdMat mat1 mat2
  | numColsMat mat1 /= numRowsMat mat2 =
    error "dotProdMat: number of columns of first matrix doesn't equal number of rows of second matrix"
  | otherwise = foldl goRow [] mat1
  where
  goRow :: [[a]] -> [a] -> [[a]]
  goRow accum rowOfMat1 =
    let newRow = fmap goCol (transpose mat2)
    in snoc accum newRow
    where
      goCol :: [a] -> a
      goCol colOfMat2 = dotProdVec rowOfMat1 colOfMat2

-- | Subtract one matrix from another.
--
-- >>> subtractMat [[1,2,3],[4,5,6]] [[7,10,20],[1,3,10]]
-- [[-6,-8,-17],[3,2,-4]]
subtractMat :: forall a. Num a => [[a]] -> [[a]] -> [[a]]
subtractMat mat1 mat2 =
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

-- | Calculate the average of a list.
--
-- >>> calcAvg [10]
-- 10.0
--
-- >>> calcAvg [1,5,9]
-- 5.0
--
-- >>> calcAvg [100, 300, 700, 900]
-- 500.0
--
-- >>> calcAvg []
-- *** Exception: calcAvg: can't calculate the average of an empty list
-- ...
calcAvg :: Fractional a => [a] -> a
calcAvg [] = error "calcAvg: can't calculate the average of an empty list"
calcAvg lst = sum lst / fromIntegral (length lst)

-- | Calculate the average for each column of a matrix.
--
-- >>> calcColAvgs [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]
-- [5.5,6.5,7.5]
--
-- >>> calcColAvgs []
-- []
calcColAvgs :: Fractional a => [[a]] -> [a]
calcColAvgs mat =
  let rows = transpose mat
  in fmap calcAvg rows


-- | Calculate the mus for a matrix (the average of each column), and then blow
-- them back, repeating them for each row.
--
-- >>> calcMatrixMu [[1,2,3],[4,5,6],[7,8,9]]
-- [[4.0,5.0,6.0],[4.0,5.0,6.0],[4.0,5.0,6.0]]
calcMatrixMu :: Fractional a => [[a]] -> [[a]]
calcMatrixMu mat = replicate (length mat) (calcColAvgs mat)

-- | Divide a matrix by a number.
--
-- >>> matDiv [ [10, 20, 30], [40, 50, 60], [70, 80, 90], [100, 110, 120] ] 10
-- [[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,9.0],[10.0,11.0,12.0]]
matDiv :: Fractional a => [[a]] -> a -> [[a]]
matDiv mat a = ffmap (/ a) mat

-- | Calculate a covariance matrix.
--
-- >>> calcCovarMatrix [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]
-- [[11.25,11.25,11.25],[11.25,11.25,11.25],[11.25,11.25,11.25]]
--
-- >>> calcCovarMatrix [[90,60,90],[90,90,30],[60,60,60],[60,60,90],[30,30,30]]
-- [[504.0,360.0,180.0],[360.0,360.0,0.0],[180.0,0.0,720.0]]
calcCovarMatrix :: Fractional a => [[a]] -> [[a]]
calcCovarMatrix mat =
  let mu = calcMatrixMu mat
      mu' = transpose mu
      mat' = transpose mat
      dMat = subtractMat mat mu
      dMat' = subtractMat mat' mu'
      res = dotProdMat dMat' dMat
      n = numSamples mat
  in matDiv res n
