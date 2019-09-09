module Civet.Model where

import Data.Sequence

data DData = DData 
    { dMin :: Double   -- Value min
    , dMax :: Double   -- Value max 
    , dCount :: Double -- Total count in histogram
    , dSum :: Double   -- Sum of values in histogram
    -- Store
    , dMaxBins :: Integer
    , dBins :: Seq Integer
    , dMinKey :: Int   -- Lower bound of key space
    , dMaxKey :: Int   -- Upper bound of key space
    } deriving Show
