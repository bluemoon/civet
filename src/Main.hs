module Main where

import qualified Data.IntMap as IntMap
import Civet.Model
import Civet.Store
import Data.Sequence
import Debug.Trace

updateBins :: DData -> Seq Integer -> DData
updateBins k v = k { dBins =  v}   

-- updateEnv key env = 

main :: IO ()
main = do
  let negInf = -1/0
  let inf = 1/0
  let env = DData negInf inf 0 0 128 (fromList [0 | _ <- [0..127]]) 0 0
  print env
  let e =  add 1.0 env
  print $ add 2.0 env
