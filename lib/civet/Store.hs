module Civet.Store where

import Civet.Model
import qualified Civet.Utils as Utils
import qualified Data.Sequence as Seq
import Debug.Trace

--  if val < -self.min_value:
--      return -int(math.ceil(math.log(-val)/self.gamma_ln)) - self.offset
--  elif val > self.min_value:
--      return int(math.ceil(math.log(val)/self.gamma_ln)) + self.offset
--  else:
--      return 0
getKey :: Double -> DData -> Int
getKey value env
  | value < -Utils.minValue = -(ceiling ( log (-value) / Utils.gammaLn ) ) - Utils.offset
  | value > Utils.minValue = (ceiling $ ((log value) / Utils.gammaLn) ) + Utils.offset 
  | otherwise = 0


growLeft :: Int -> DData -> DData
growLeft key env
  | (dMinKey env) < key || Seq.length (dBins env) >= 2048 = env
  | otherwise = let 
      minKey = if (dMaxKey env) - key >= 2048 then (dMaxKey env) - key + 1 else 1
      -- prepend the zeros to the beginning
      array = Seq.fromList [0 | x <- [0..(minKey - key)]]
      newEnv = env { dBins = array <> (dBins env) }
  in newEnv
-- growRight :: Int -> DData -> DData
-- growRight = _

-- idx = max(0, key - self.min_key)
-- self.bins[idx] += 1
-- self.count += 1
add :: Double -> DData -> DData
add value env
  | (dCount env) == 0 = let 
    minKey = key - Seq.length (dBins env) + 1
    newEnv = env { 
      dMaxKey = key, 
      dMinKey = minKey,
      dBins = Seq.adjust' 
        (\x -> x + 1) 
        (index minKey)
        (dBins env)
    }
    in newEnv

  | key < (dMinKey env) = 
    growLeft key env
  | otherwise = env
  -- I need to update dCount here as well += 1
  -- updateBins env _
  where
    key = getKey value env
    index val = max 0 key - val :: Int
    f k x = Just (x + 1)