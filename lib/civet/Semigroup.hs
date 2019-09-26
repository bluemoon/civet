module Civet.Semigroup where

import Civet.Model

instance Semigroup DData where
    DData dMin dMax dCount dSum dMaxBins dBins dMinKey dMaxKey <> DData dMin' dMax' dCount' dSum' dMaxBins' dBins' dMinKey' dMaxKey' = undefined
