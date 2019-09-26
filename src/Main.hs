module Main where

import qualified Data.IntMap as IntMap
import Civet.Model
import Civet.Store
import Data.Sequence
import Debug.Trace

updateBins :: DData -> Seq Integer -> DData
updateBins k v = k { dBins =  v}

buildFromList :: DData -> [Double] -> DData
buildFromList env []     = env
buildFromList env (x:xs) = buildFromList (add x env) xs

main :: IO ()
main = do
  let negInf = -1/0
  let inf = 1/0
  -- Setup the environment
  let env = DData negInf inf 0 0 128 (fromList [0 | _ <- [0..127]]) 0 0
  let histData = [1.0, 3.0, 4.0, 25.0, 300.0, 50000.0]
  let a = buildFromList env histData
  print $ a
  print $ quantile 0.0 a
