module Civet.Store where

import Civet.Model
import qualified Civet.Utils as Utils

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

-- idx = max(0, key - self.min_key)
-- self.bins[idx] += 1
-- self.count += 1
add :: Double -> DData -> DData
add value env = 
  -- I need to update dCount here as well += 1
  -- updateBins env _
  env
  where
    key = getKey value env
    index = max 0 key - (dMinKey env) :: Int
    f k x = Just (x + 1)