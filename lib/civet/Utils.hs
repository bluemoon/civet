module Civet.Utils where

minValue :: Double
minValue = 1.0e-9

alpha :: Double
alpha = 0.01

_gamma :: Double -> Double
_gamma alpha = 1 +  2 * alpha / (1 - alpha)

gamma = _gamma alpha

-- self.gamma_ln = math.log1p(2*alpha/(1-alpha))
_gammaLn :: Double -> Double
_gammaLn alpha = log $ 1 + (2 * alpha / (1 - alpha))

gammaLn :: Double
gammaLn = _gammaLn alpha

-- self.offset = -int(math.ceil(math.log(min_value)/self.gamma_ln)) + 1
_offset :: Double -> Int
_offset alpha = -(ceiling (log minValue / gammaLn)) + 1

offset = _offset alpha
