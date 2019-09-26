module Civet.Model where

import Data.Sequence

data DData = DData
    { -- | Value min
      dMin :: Double
      -- | Value max
    , dMax :: Double
      -- | Total count in histogram
    , dCount :: Int
      -- | Sum of values in histogram
    , dSum :: Double
      -- | Max number of bins
    , dMaxBins :: Integer
      -- | The structure that stores the histogram
    , dBins :: Seq Integer
      -- | Lower bound of key space
    , dMinKey :: Int
      -- | Upper bound of key space
    , dMaxKey :: Int
    } deriving Show
