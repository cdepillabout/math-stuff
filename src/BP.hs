-- https://medium.com/binaryandmore/beginners-guide-to-deriving-and-implementing-backpropagation-e3c1a5a1e536

module BP where

import Data.Map

sigmoid :: Floating x => x -> x
sigmoid x = 1 / (1 + exp (-x))

sigmoidPrime :: Floating x => x -> x
sigmoidPrime x = sigmoid x * (1 - sigmoid x)

data Params = Params
  { layerSizes :: [Int] -- ^ Number of nodes in each layer.
  , parameters :: Map String (Matrix Float)
  , derivates :: Map String Float
  }

lastLayerIndexParams :: Params -> Int
lastLayerIndexParams Params{..} = lastLayerIndex layerSizes

lastLayerIndex :: [Int] -> Int
lastLayerIndex x = length x - 1

createParameters :: forall m. MonadRandom m => [Int] -> m (Map String (Matrix Float))
createParameters architecture =
  foldM go empty [1 .. lastLayerIndex architecture]
  where
    go :: Map String (Matrix Float) -> Int -> m (Map String (Matrix Float))
    go params i =
      insert ("W" <> show i) params

initParams :: MonadRandom m => [Int] -> m Params
initParams architecture = do
  params <- createParameters architecture
  Params
    { layerSizes = architecture
    , parameters = params
    , derivaties = empty
    }
